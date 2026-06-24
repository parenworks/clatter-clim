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

(defun ensure-buffer (frame network name kind)
  "Return the buffer named NAME on NETWORK, creating it with KIND if needed.
Buffer identity is (network . name), so the same channel on two networks maps
to two distinct buffers."
  (or (find-if (lambda (b)
                 (and (eq (buffer-network b) network)
                      (string-equal (buffer-name b) name)))
               (app-buffers frame))
      (let ((b (make-instance 'buffer :name name :kind kind :network network)))
        (setf (app-buffers frame) (append (app-buffers frame) (list b)))
        (unless (app-current frame)
          (setf (app-current frame) b))
        b)))

;;; ----------------------------------------------------------------------
;;; The mailbox
;;; ----------------------------------------------------------------------

;;; We wake the frame with our own event rather than EXECUTE-FRAME-COMMAND.
;;; Routing a command from another thread makes McCLIM throw it into the
;;; active command reader, which echoes it into the interactor (the
;;; CLATTER-CLIM::COM-DRAIN spam).  A plain window-manager-event is handled
;;; directly by HANDLE-EVENT and never touches the command reader.
(defclass ui-drain-event (clim:window-manager-event)
  ((sheet :initarg :sheet :reader clim:event-sheet)
   (frame :initarg :frame :reader ui-drain-event-frame)))

(defmethod clim:handle-event (sheet (event ui-drain-event))
  (declare (ignore sheet))
  (drain-mailbox (ui-drain-event-frame event)))

(defun post-update (frame update)
  "Called on the IRC reader thread.  Enqueue UPDATE and wake the frame.
UPDATE is a list whose head is a keyword (see APPLY-UPDATE).  Thread-safe:
this only locks the mailbox and posts an event; it never touches panes."
  (bt:with-lock-held ((app-mailbox-lock frame))
    (push update (app-mailbox frame)))
  (let ((sheet (clim:frame-top-level-sheet frame)))
    (when sheet
      (clim:queue-event sheet (make-instance 'ui-drain-event
                                             :sheet sheet :frame frame)))))

(defun drain-mailbox (frame)
  "Called on the frame thread by HANDLE-EVENT for UI-DRAIN-EVENT.
Apply every pending update, then repaint."
  (let ((items nil))
    (bt:with-lock-held ((app-mailbox-lock frame))
      (setf items (nreverse (app-mailbox frame))
            (app-mailbox frame) '()))
    (dolist (update items)
      (apply-update frame update))
    (when items
      (redisplay-current frame))))

(defun apply-update (frame update)
  "Mutate the model for one UPDATE.  Frame thread only.  Every UPDATE carries
the NETWORK it belongs to right after the KIND keyword, so buffers are resolved
within the right server."
  (destructuring-bind (kind network &rest args) update
    (ecase kind
      (:line
       (destructuring-bind (target line) args
         (unless (nick-ignored-p frame (irc-line-nick line))
           (let ((b (ensure-buffer frame network target :channel)))
             (buffer-add-line b line)
             (note-activity frame b line)
             (log-line frame network target line)))))
      (:system
       (destructuring-bind (target text) args
         (let ((line (make-irc-line :system nil text)))
           (buffer-add-line (ensure-buffer frame network target :server) line)
           (log-line frame network target line))))
      (:info
       ;; A server-originated informational line (whois, MOTD, lusers, ...).
       ;; SCOPE is :server (NETWORK's server buffer) or :current (wherever the
       ;; user is looking, e.g. the channel they ran /whois from) as long as it
       ;; belongs to this network, else NETWORK's server buffer.
       (destructuring-bind (scope text) args
         (let* ((server (network-label network))
                (b (ecase scope
                     (:current (let ((cur (app-current frame)))
                                 (if (and cur (eq (buffer-network cur) network))
                                     cur
                                     (ensure-buffer frame network server :server))))
                     (:server (ensure-buffer frame network server :server))))
                (line (make-irc-line :system nil text)))
           (buffer-add-line b line)
           (log-line frame network (buffer-name b) line))))
      (:join
       (destructuring-bind (channel nick) args
         (let ((line (make-irc-line :join nick (format nil "~A has joined" nick))))
           (buffer-add-line (ensure-buffer frame network channel :channel) line)
           (log-line frame network channel line))))
      (:part
       (destructuring-bind (channel nick reason) args
         (let ((line (make-irc-line :part nick
                                    (format nil "~A has left~@[ (~A)~]" nick reason))))
           (buffer-add-line (ensure-buffer frame network channel :channel) line)
           (log-line frame network channel line))))
      (:topic
       (destructuring-bind (channel text) args
         (setf (buffer-topic (ensure-buffer frame network channel :channel)) text)))
      (:names
       (destructuring-bind (channel nicks) args
         (setf (buffer-users (ensure-buffer frame network channel :channel)) nicks)))
      (:dcc-offer
       ;; An incoming DCC offer, rendered as a clickable :dcc-offer row in the
       ;; network's server buffer.  OFFER is a DCC-OFFER struct (model.lisp).
       (destructuring-bind (offer text) args
         (let ((b (ensure-buffer frame network (network-label network) :server)))
           (buffer-add-line b (make-irc-line :dcc-offer nil text (get-universal-time) offer))
           ;; Offers should draw the eye even when another buffer is current.
           (unless (eq b (app-current frame))
             (incf (buffer-unread b))
             (setf (buffer-ping b) t)))))
      (:dcc-chat-line
       ;; A line received on a DCC CHAT.  CHAT is the clatter-irc dcc-chat; its
       ;; peer nick names the buffer.  Storing CHAT lets SAY reply over it.
       (destructuring-bind (chat text) args
         (let* ((nick (irc:dcc-connection-nick chat))
                (b (ensure-buffer frame network (format nil "=~A" nick) :dcc))
                (line (make-irc-line :privmsg nick text)))
           (setf (buffer-dcc b) chat)
           (buffer-add-line b line)
           (note-activity frame b line))))
      (:disconnected
       (dolist (b (app-buffers frame))
         (when (eq (buffer-network b) network)
           (buffer-add-line b (make-irc-line :system nil "disconnected"))
           (when (eq (buffer-kind b) :channel)
             (setf (buffer-users b) '()))))))))

;;; ----------------------------------------------------------------------
;;; Server numeric replies
;;; ----------------------------------------------------------------------

(defparameter *whois-numerics*
  '(301 307 311 312 313 314 317 318 319 320 330 338 369 378 379 671)
  "WHOIS / WHOWAS reply codes, routed to the current buffer.")

(defparameter *numeric-skip*
  '(005 324 329 332 333)
  "Numerics handled elsewhere (topic, modes) or too noisy to print (ISUPPORT).
NAMREPLY/ENDOFNAMES (353/366) are handled separately to refresh the nick list.")

(defun msg-time (msg)
  "Universal time for MSG: the IRCv3 server-time tag when present (so replayed
history and bouncer playback show when a line was actually said), else now."
  (or (and msg (irc:get-server-time (irc:message-tags msg)))
      (get-universal-time)))

(defun numeric-text (msg)
  "Readable text for a numeric reply: every parameter after the first (which is
our own nick) joined with spaces."
  (string-trim " " (format nil "~{~A~^ ~}" (rest (irc:message-params msg)))))

(defun handle-dcc-offer (frame network sender args)
  "IRC-thread handler for an incoming CTCP DCC from SENDER.  ARGS is everything
after 'DCC ', e.g. 'CHAT chat <ip> <port>' or 'SEND <file> <ip> <port> <size>'.
Registers the offer with NETWORK's DCC manager and posts a clickable offer row."
  (multiple-value-bind (type rest) (%split-first args)
    (let ((conn (irc:dcc-handle-offer (network-dcc network) sender type rest)))
      (when conn
        (let* ((id (irc:dcc-connection-id conn))
               (text (if (string-equal type "SEND")
                         (format nil "DCC SEND from ~A: ~A (~A bytes) [#~A]"
                                 sender (irc:dcc-send-filename conn)
                                 (irc:dcc-send-filesize conn) id)
                         (format nil "DCC CHAT from ~A [#~A]" sender id))))
          (post-update frame (list :dcc-offer network
                                   (make-dcc-offer network conn) text)))))))

;;; ----------------------------------------------------------------------
;;; Hook installation: clatter-irc events -> mailbox updates
;;; ----------------------------------------------------------------------

(defun install-irc-hooks (frame network)
  "Wire clatter-irc hooks so inbound events on NETWORK post mailbox updates for
FRAME.  Every update is tagged with NETWORK so the bridge resolves buffers
within the right server.  The hook lambda lists match the signatures documented
in clatter-irc."
  (let ((conn (network-connection network)))
    (labels ((server-name () (network-label network))
             ;; Autojoin and the persisted autojoin list belong to the single
             ;; configured server, so only touch them on the network whose label
             ;; matches the config; a second network never joins or rewrites it.
             (config-network-p ()
               (string-equal (network-label network) (config-server *config*)))
             ;; Membership is owned by clatter-irc, which tracks channel-users
             ;; (including op/voice prefixes).  We just mirror its current view
             ;; into the buffer's nick list via a :names update.
             (refresh-channel (channel)
               (let ((ch (irc:find-channel conn channel)))
                 (when ch
                   (post-update frame (list :names network (irc:channel-name ch)
                                            (irc:channel-user-nicks-with-prefix ch)))
                   (let ((topic (irc:channel-topic ch)))
                     (when topic
                       (post-update frame (list :topic network (irc:channel-name ch) topic)))))))
             (refresh-all ()
               (dolist (ch (irc:joined-channels conn))
                 (post-update frame (list :names network (irc:channel-name ch)
                                          (irc:channel-user-nicks-with-prefix ch))))))
      (irc:add-hook conn 'irc:on-connect
        (lambda (c)
          (declare (ignore c))
          (post-update frame (list :system network (server-name) "connected"))
          ;; Auto-join the persisted channels so you do not re-join by hand.
          (when (config-network-p)
            (dolist (chan (config-autojoin *config*))
              (irc:join conn chan)))))
      (irc:add-hook conn 'irc:on-disconnect
        (lambda (c)
          (declare (ignore c))
          (post-update frame (list :disconnected network))))
      (irc:add-hook conn 'irc:on-privmsg
        (lambda (c msg sender target text)
          (declare (ignore c))
          ;; A message to us (target = our nick) opens a query keyed by sender.
          (let ((buf (if (irc:channel-name-p target) target sender)))
            (post-update frame (list :line network buf
                                     (make-irc-line :privmsg sender text (msg-time msg)))))))
      (irc:add-hook conn 'irc:on-notice
        (lambda (c msg sender target text)
          (declare (ignore c))
          (let ((buf (if (irc:channel-name-p target) target (server-name))))
            (post-update frame (list :line network buf
                                     (make-irc-line :notice sender text (msg-time msg)))))))
      (irc:add-hook conn 'irc:on-ctcp
        (lambda (c msg sender target command args)
          (declare (ignore c))
          ;; Render /me (CTCP ACTION) as an action line; ignore other CTCP.
          (when (string-equal command "ACTION")
            (let ((buf (if (irc:channel-name-p target) target sender)))
              (post-update frame
                           (list :line network buf
                                 (make-irc-line :privmsg sender
                                                (format nil "* ~A ~A" sender args)
                                                (msg-time msg))))))))
      (irc:add-hook conn 'irc:on-dcc
        (lambda (c msg sender args)
          (declare (ignore c msg))
          (handle-dcc-offer frame network sender args)))
      (irc:add-hook conn 'irc:on-join
        (lambda (c msg nick channel)
          (declare (ignore c msg))
          ;; When we join a channel ourselves, remember it for next time.
          (when (and (config-network-p)
                     (irc:nick-equal nick (irc:connection-nick conn)))
            (config-add-autojoin channel))
          (post-update frame (list :join network channel nick))
          (refresh-channel channel)))
      (irc:add-hook conn 'irc:on-part
        (lambda (c msg nick channel reason)
          (declare (ignore c msg))
          ;; When we part a channel ourselves, stop auto-joining it.
          (when (and (config-network-p)
                     (irc:nick-equal nick (irc:connection-nick conn)))
            (config-remove-autojoin channel))
          (post-update frame (list :part network channel nick reason))
          (refresh-channel channel)))
      (irc:add-hook conn 'irc:on-quit
        (lambda (c msg nick reason)
          (declare (ignore c msg nick reason))
          (refresh-all)))
      (irc:add-hook conn 'irc:on-nick
        (lambda (c msg old-nick new-nick)
          (declare (ignore c msg old-nick new-nick))
          (refresh-all)))
      (irc:add-hook conn 'irc:on-topic
        (lambda (c msg setter channel topic)
          (declare (ignore c msg setter))
          (post-update frame (list :topic network channel topic))))
      ;; Numeric replies.  353 = RPL_NAMREPLY, 366 = RPL_ENDOFNAMES: clatter-irc
      ;; has already folded the names into channel-users, so we just refresh the
      ;; nick list.  WHOIS replies go to the current buffer (where the user ran
      ;; whois); everything else lands in the server buffer so it is no longer
      ;; silent.  Structural/noisy numerics are skipped.
      (irc:add-hook conn 'irc:on-numeric
        (lambda (c msg code name)
          (declare (ignore c name))
          (cond
            ((member code '(353 366)) (refresh-all))
            ((member code *numeric-skip*) nil)
            (t (let ((text (numeric-text msg)))
                 (when (plusp (length text))
                   (post-update frame
                                (list :info network
                                      (if (member code *whois-numerics*)
                                          :current :server)
                                      text))))))))
      (irc:add-hook conn 'irc:on-error
        (lambda (c msg text)
          (declare (ignore c msg))
          (post-update frame (list :system network (server-name)
                                   (format nil "error: ~A" text))))))))
