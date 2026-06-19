;;;; bridge.lisp - the IRC-thread to CLIM-thread bridge
;;;;
;;;; This is the crux of the whole client.  clatter-irc fires its hooks on a
;;;; background reader thread.  McCLIM owns its own event loop on the frame
;;;; thread, and panes must only be mutated and repainted there.
;;;;
;;;; The safe, backend-agnostic bridge is CLIM:EXECUTE-FRAME-COMMAND.  When it
;;;; is called from a thread other than the frame's process it does not run
;;;; the command inline; it enqueues an EXECUTE-COMMAND-EVENT on the frame's
;;;; sheet event queue (see McCLIM frames.lisp).  The frame thread then runs
;;;; the command in its normal loop.  This works identically on CLX,
;;;; mcclim-charmed and clim-clog because all three share that event queue.
;;;;
;;;; So: hooks only ever PUSH an update onto the mailbox (under a lock) and
;;;; poke the frame with COM-DRAIN.  All model mutation happens in
;;;; DRAIN-MAILBOX on the frame thread.

(in-package #:clatter-clim)

;;; ----------------------------------------------------------------------
;;; Buffer lookup / creation (frame thread only)
;;; ----------------------------------------------------------------------

(defun ensure-buffer (frame name kind)
  "Return the buffer named NAME, creating it with KIND if needed."
  (or (find name (app-buffers frame) :key #'buffer-name :test #'string-equal)
      (let ((b (make-instance 'buffer :name name :kind kind)))
        (setf (app-buffers frame) (append (app-buffers frame) (list b)))
        (unless (app-current frame)
          (setf (app-current frame) b))
        b)))

;;; ----------------------------------------------------------------------
;;; The mailbox
;;; ----------------------------------------------------------------------

(defun post-update (frame update)
  "Called on the IRC reader thread.  Enqueue UPDATE and wake the frame.
UPDATE is a list whose head is a keyword (see APPLY-UPDATE)."
  (bt:with-lock-held ((app-mailbox-lock frame))
    (push update (app-mailbox frame)))
  ;; Thread-safe: this enqueues an event for the frame thread, it does not
  ;; touch panes from here.
  (execute-frame-command frame (list 'com-drain)))

(defun drain-mailbox (frame)
  "Called on the frame thread by COM-DRAIN.  Apply every pending update."
  (let ((items nil))
    (bt:with-lock-held ((app-mailbox-lock frame))
      (setf items (nreverse (app-mailbox frame))
            (app-mailbox frame) '()))
    (dolist (update items)
      (apply-update frame update))
    (when items
      (redisplay-current frame))))

(defun apply-update (frame update)
  "Mutate the model for one UPDATE.  Frame thread only."
  (destructuring-bind (kind &rest args) update
    (ecase kind
      (:line
       (destructuring-bind (target line) args
         (buffer-add-line (ensure-buffer frame target :channel) line)))
      (:system
       (destructuring-bind (target text) args
         (buffer-add-line (ensure-buffer frame target :server)
                          (make-line :system nil text))))
      (:join
       (destructuring-bind (channel nick) args
         (let ((b (ensure-buffer frame channel :channel)))
           (pushnew nick (buffer-users b) :test #'string-equal)
           (buffer-add-line b (make-line :join nick (format nil "~A has joined" nick))))))
      (:part
       (destructuring-bind (channel nick reason) args
         (let ((b (ensure-buffer frame channel :channel)))
           (setf (buffer-users b)
                 (remove nick (buffer-users b) :test #'string-equal))
           (buffer-add-line b (make-line :part nick
                                         (format nil "~A has left~@[ (~A)~]" nick reason))))))
      (:topic
       (destructuring-bind (channel text) args
         (setf (buffer-topic (ensure-buffer frame channel :channel)) text)))
      (:names
       (destructuring-bind (channel nicks) args
         (setf (buffer-users (ensure-buffer frame channel :channel)) nicks))))))

;;; ----------------------------------------------------------------------
;;; Hook installation: clatter-irc events -> mailbox updates
;;; ----------------------------------------------------------------------

(defun install-irc-hooks (frame conn)
  "Wire clatter-irc hooks so inbound events post mailbox updates for FRAME.
The hook lambda lists match the signatures documented in clatter-irc."
  (flet ((server-name ()
           (or (irc:connection-server conn) "server")))
    (irc:add-hook conn 'irc:on-connect
      (lambda (c)
        (declare (ignore c))
        (post-update frame (list :system (server-name) "connected"))))
    (irc:add-hook conn 'irc:on-privmsg
      (lambda (c msg sender target text)
        (declare (ignore c msg))
        ;; A message to us (target = our nick) opens a query keyed by sender.
        (let ((buf (if (irc:channel-name-p target) target sender)))
          (post-update frame (list :line buf (make-line :privmsg sender text))))))
    (irc:add-hook conn 'irc:on-notice
      (lambda (c msg sender target text)
        (declare (ignore c msg))
        (let ((buf (if (irc:channel-name-p target) target (server-name))))
          (post-update frame (list :line buf (make-line :notice sender text))))))
    (irc:add-hook conn 'irc:on-join
      (lambda (c msg nick channel)
        (declare (ignore c msg))
        (post-update frame (list :join channel nick))))
    (irc:add-hook conn 'irc:on-part
      (lambda (c msg nick channel reason)
        (declare (ignore c msg))
        (post-update frame (list :part channel nick reason))))
    (irc:add-hook conn 'irc:on-topic
      (lambda (c msg setter channel topic)
        (declare (ignore c msg setter))
        (post-update frame (list :topic channel topic))))
    (irc:add-hook conn 'irc:on-error
      (lambda (c msg text)
        (declare (ignore c msg))
        (post-update frame (list :system (server-name) (format nil "error: ~A" text)))))))
