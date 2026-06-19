;;;; frame.lisp - the application frame, panes, and display functions

(in-package #:clatter-clim)

;;; Connection defaults.  parensmith is the throwaway test nick so a test
;;; connect never collides with the glenneth bouncer session.  A future
;;; config file (see ROADMAP) overrides these at boot, and the connect
;;; dialog pre-fills from them.
(defvar *default-server* "irc.libera.chat")
(defvar *default-port* 6697)
(defvar *default-nick* "parensmith")

;;; Dark theme palette, modelled on quaestor.  Every pane is given an
;;; explicit foreground and background so there is no white flash-bang on
;;; any backend; display functions draw with these inks rather than the
;;; default black-on-white.
(defparameter *colour-bg-main*    (make-rgb-color 0.11 0.12 0.14))
(defparameter *colour-bg-accent* (make-rgb-color 0.13 0.14 0.17))
(defparameter *colour-bg-header* (make-rgb-color 0.15 0.16 0.19))
(defparameter *colour-fg-default* (make-rgb-color 0.85 0.87 0.90))
(defparameter *colour-heading*    (make-rgb-color 0.65 0.75 0.90))
(defparameter *colour-muted*      (make-rgb-color 0.50 0.52 0.56))
(defparameter *colour-self*       (make-rgb-color 0.40 0.70 0.95))

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
                :end-of-line-action :allow
                :foreground *colour-fg-default*
                :background *colour-bg-accent*)
   (messages    :application
                :display-function 'display-messages
                :incremental-redisplay t
                :scroll-bars t
                :end-of-line-action :wrap*
                :foreground *colour-fg-default*
                :background *colour-bg-main*)
   (nick-list   :application
                :display-function 'display-nick-list
                :scroll-bars :vertical
                :foreground *colour-fg-default*
                :background *colour-bg-accent*)
   (status      :application
                :display-function 'display-status
                :scroll-bars nil
                :height 22
                :foreground *colour-fg-default*
                :background *colour-bg-header*)
   (input       :interactor
                :scroll-bars nil
                :height 44
                :foreground *colour-fg-default*
                :background *colour-bg-header*)
   ;; Toolbar gadgets.  pane-frame recovers the frame inside the callback,
   ;; which runs on the frame thread; the commands default their arguments
   ;; (see commands.lisp) so a bare click connects as *default-nick*.
   (connect-button
    (make-pane 'push-button
               :label "Connect"
               :foreground *colour-fg-default*
               :background *colour-bg-header*
               :activate-callback
               (lambda (g) (execute-frame-command (pane-frame g) (list 'com-connect)))))
   (disconnect-button
    (make-pane 'push-button
               :label "Disconnect"
               :foreground *colour-fg-default*
               :background *colour-bg-header*
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
                                             *colour-heading*
                                             *colour-fg-default*))
          (format pane "~:[  ~;> ~]~A~%" current-p (buffer-name b)))))))

(defun display-messages (frame pane)
  (let ((b (app-current frame)))
    (when b
      (loop for line across (buffer-lines b) do
        (ecase (irc-line-kind line)
          ((:privmsg :notice)
           (write-string "<" pane)
           (with-drawing-options (pane :ink *colour-self*)
             (present (irc-line-nick line) 'nick :stream pane))
           (format pane "> ~A~%" (irc-line-text line)))
          ((:join :part :quit :topic :system)
           (with-drawing-options (pane :ink *colour-muted*)
             (format pane "-!- ~A~%" (irc-line-text line)))))))))

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
