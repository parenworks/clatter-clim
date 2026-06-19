;;;; frame.lisp - the application frame, panes, and display functions

(in-package #:clatter-clim)

;;; Connection defaults.  parensmith is the throwaway test nick so a test
;;; connect never collides with the glenneth bouncer session.  A future
;;; config file (see ROADMAP) overrides these at boot, and the connect
;;; dialog pre-fills from them.
(defvar *default-server* "irc.libera.chat")
(defvar *default-port* 6697)
(defvar *default-nick* "parensmith")

(define-application-frame clatter-clim ()
  ((connection   :initform nil :accessor app-connection)
   (buffers      :initform '() :accessor app-buffers)
   (current      :initform nil :accessor app-current)
   ;; The mailbox is the only state the IRC reader thread writes to, under
   ;; MAILBOX-LOCK.  COM-DRAIN empties it on the frame thread.
   (mailbox      :initform '() :accessor app-mailbox)
   (mailbox-lock :initform (bt:make-lock "clatter-clim-mailbox")
                 :reader app-mailbox-lock))
  (:menu-bar nil)
  (:panes
   (buffer-list :application
                :display-function 'display-buffer-list
                :scroll-bars :vertical
                :end-of-line-action :allow)
   (messages    :application
                :display-function 'display-messages
                :incremental-redisplay t
                :scroll-bars t
                :end-of-line-action :wrap*)
   (nick-list   :application
                :display-function 'display-nick-list
                :scroll-bars :vertical)
   (status      :application
                :display-function 'display-status
                :scroll-bars nil
                :height 22)
   (input       :interactor
                :scroll-bars nil
                :height 44)
   ;; Toolbar gadgets.  pane-frame recovers the frame inside the callback,
   ;; which runs on the frame thread; the commands default their arguments
   ;; (see commands.lisp) so a bare click connects as *default-nick*.
   (connect-button
    (make-pane 'push-button
               :label "Connect"
               :activate-callback
               (lambda (g) (execute-frame-command (pane-frame g) (list 'com-connect)))))
   (disconnect-button
    (make-pane 'push-button
               :label "Disconnect"
               :activate-callback
               (lambda (g) (execute-frame-command (pane-frame g) (list 'com-disconnect))))))
  (:layouts
   (default
    (vertically ()
      (horizontally ()
        connect-button
        disconnect-button)
      (horizontally ()
        (1/6 buffer-list)
        (2/3 (vertically ()
               status
               messages))
        (1/6 nick-list))
      input))))

;;; ----------------------------------------------------------------------
;;; Display functions.  Each reads the model and paints one pane.  They are
;;; backend-agnostic: identical code drives CLX, the terminal and the browser.
;;; ----------------------------------------------------------------------

(defun display-buffer-list (frame pane)
  (dolist (b (app-buffers frame))
    (let ((current-p (eq b (app-current frame))))
      (with-output-as-presentation (pane b 'buffer)
        (with-drawing-options (pane :ink (if current-p
                                             +royalblue+
                                             +foreground-ink+))
          (format pane "~:[  ~;> ~]~A~%" current-p (buffer-name b)))))))

(defun display-messages (frame pane)
  (let ((b (app-current frame)))
    (when b
      (loop for line across (buffer-lines b) do
        (ecase (line-kind line)
          ((:privmsg :notice)
           (write-string "<" pane)
           (present (line-nick line) 'nick :stream pane)
           (format pane "> ~A~%" (line-text line)))
          ((:join :part :quit :topic :system)
           (with-drawing-options (pane :ink +gray50+)
             (format pane "-!- ~A~%" (line-text line)))))))))

(defun display-nick-list (frame pane)
  (let ((b (app-current frame)))
    (when (and b (eq (buffer-kind b) :channel))
      (dolist (n (buffer-users b))
        (present n 'nick :stream pane)
        (terpri pane)))))

(defun display-status (frame pane)
  (let ((conn (app-connection frame))
        (b (app-current frame)))
    (format pane "~A | ~A | ~A"
            (if conn (or (irc:connection-nick conn) "?") "(disconnected)")
            (if b (buffer-name b) "(no buffer)")
            (if b (buffer-topic b) ""))))

(defun redisplay-current (frame)
  "Force a repaint of every model-backed pane on the frame thread."
  (dolist (name '(buffer-list status messages nick-list))
    (let ((pane (find-pane-named frame name)))
      (when pane
        (redisplay-frame-pane frame pane :force-p t)))))
