;;;; frame.lisp - the application frame, panes, and display functions

(in-package #:clatter-clim)

;;; Connection defaults live in config.lisp (*default-server*, *default-port*,
;;; *default-nick*), seeded from the persisted config at startup.

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
(defparameter *colour-highlight*  (make-rgb-color 0.98 0.85 0.45)
  "Ink for a line that mentions our own nick (the ping highlight).")

;;; Per-nick colouring: each speaker keeps a stable colour, hashed from the
;;; bare nick (mode prefixes such as @ and + stripped so an op and a regular
;;; speaker map to the same colour).
(defparameter *nick-colours*
  (vector (make-rgb-color 0.40 0.70 0.95)
          (make-rgb-color 0.30 0.78 0.50)
          (make-rgb-color 0.95 0.60 0.25)
          (make-rgb-color 0.70 0.55 0.85)
          (make-rgb-color 0.95 0.45 0.45)
          (make-rgb-color 0.45 0.80 0.80)
          (make-rgb-color 0.85 0.75 0.45)
          (make-rgb-color 0.65 0.85 0.55)))

(defun nick-ink (nick)
  "Return the stable colour for NICK."
  (let ((bare (string-left-trim "@%+~&!." nick)))
    (aref *nick-colours*
          (mod (sxhash (string-downcase bare)) (length *nick-colours*)))))

(defun mentions-p (text nick)
  "True if TEXT mentions NICK (case-insensitive)."
  (and nick (plusp (length nick))
       (search (string-downcase nick) (string-downcase text))
       t))

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
  (:top-level (default-frame-top-level :prompt "> "))
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
                :height 24 :min-height 24 :max-height 24
                :foreground *colour-fg-default*
                :background *colour-bg-header*)
   (input       :interactor
                :scroll-bars nil
                :height 72 :min-height 48 :max-height 96
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
               :width 120 :max-width 160 :height 28 :max-height 28
               ;; Call the command directly on the frame thread.  We cannot go
               ;; through execute-frame-command here: the free-form input model
               ;; blocks in ACCEPT, so a queued command would not run until the
               ;; next Return.  The callback already runs on the frame thread.
               :activate-callback
               (lambda (g) (let ((*application-frame* (pane-frame g))) (com-connect)))))
   (disconnect-button
    (make-pane 'push-button
               :label "Disconnect"
               :foreground *colour-fg-default*
               :background *colour-bg-header*
               :width 120 :max-width 160 :height 28 :max-height 28
               :activate-callback
               (lambda (g) (let ((*application-frame* (pane-frame g))) (com-disconnect))))))
  (:layouts
   (default
    (vertically ()
      (horizontally ()
        connect-button
        disconnect-button
        +fill+)
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
  (let* ((b (app-current frame))
         (me (let ((conn (app-connection frame)))
               (and conn (irc:connection-nick conn)))))
    (when b
      (loop for line across (buffer-lines b) do
        (ecase (irc-line-kind line)
          ((:privmsg :notice)
           (let* ((nick (irc-line-nick line))
                  (text (irc-line-text line))
                  ;; Highlight when someone else mentions our nick.
                  (ping (and me nick
                             (not (irc:nick-equal nick me))
                             (mentions-p text me))))
             (write-string "<" pane)
             (with-drawing-options (pane :ink (nick-ink nick))
               (present nick 'nick :stream pane))
             (write-string "> " pane)
             (if ping
                 (with-drawing-options (pane :ink *colour-highlight*)
                   (format pane "~A~%" text))
                 (format pane "~A~%" text))))
          ((:join :part :quit :topic :system)
           (with-drawing-options (pane :ink *colour-muted*)
             (format pane "-!- ~A~%" (irc-line-text line)))))))))

(defun display-nick-list (frame pane)
  (let ((b (app-current frame)))
    (when (and b (eq (buffer-kind b) :channel))
      (dolist (n (buffer-users b))
        (with-drawing-options (pane :ink (nick-ink n))
          (present n 'nick :stream pane))
        (terpri pane)))))

(defun display-status (frame pane)
  (let* ((conn (app-connection frame))
         (b (app-current frame))
         (connected (and conn (irc:connectedp conn))))
    (format pane "~A | ~A | ~A"
            (if connected (irc:connection-nick conn) "(disconnected)")
            (if b (buffer-name b) "(no buffer)")
            (if b (buffer-topic b) ""))))

(defun redisplay-current (frame)
  "Force a repaint of every model-backed pane on the frame thread."
  (dolist (name '(buffer-list status messages nick-list))
    (let ((pane (find-pane-named frame name)))
      (when pane
        (redisplay-frame-pane frame pane :force-p t)))))

;;; ----------------------------------------------------------------------
;;; Nick completion.  Commands that read a NICK argument (Whois, Query)
;;; complete from the current channel's members.  Arbitrary nicks are still
;;; allowed (allow-any-input), so you can whois someone not in the channel.
;;; ----------------------------------------------------------------------

(defun current-nicks (&optional (frame *application-frame*))
  "Bare nicks in the current channel buffer, prefixes stripped, for completion."
  (let ((b (and frame (app-current frame))))
    (when b
      (mapcar (lambda (n) (string-left-trim "@%+~&!." n))
              (buffer-users b)))))

(define-presentation-method accept ((type nick) stream (view textual-view) &key)
  (let ((nicks (current-nicks)))
    (if nicks
        (values
         (complete-input stream
                         (lambda (so action)
                           ;; NICKS are bare strings, so identity keys; the
                           ;; default keys expect (name value) lists.
                           (complete-from-possibilities so nicks '(#\Space)
                                                        :action action
                                                        :name-key #'identity
                                                        :value-key #'identity))
                         :allow-any-input t
                         :partial-completers '(#\Space)))
        (accept 'string :stream stream :prompt nil :view view))))

;;; ----------------------------------------------------------------------
;;; Free-form input.  Like every other IRC client: plain text is sent to the
;;; current buffer, and a leading / introduces a command.  We override
;;; read-frame-command to read a whole line (spaces and all) and turn it into
;;; the right command.  The :around method on application-frame still runs, so
;;; button-posted commands via the command queue keep working.
;;; ----------------------------------------------------------------------

(defun %split-last-token (s)
  "Split S into (values prefix last-token), where PREFIX keeps its trailing
space so PREFIX + TOKEN reconstructs S.  The token is the run after the last
space, i.e. the word TAB should complete."
  (let ((sp (position #\Space s :from-end t)))
    (if sp
        (values (subseq s 0 (1+ sp)) (subseq s (1+ sp)))
        (values "" s))))

(defun complete-chat-token (so action frame)
  "complete-input completer: complete only the last whitespace-delimited token
of SO against the current channel's nicks, leaving any preceding text intact.
A unique match gains a trailing \": \" at line start or a space elsewhere, the
usual IRC convention.  Ambiguous matches complete to the common prefix."
  (multiple-value-bind (prefix token) (%split-last-token so)
    (let ((nicks (current-nicks frame)))
      (if (and nicks (plusp (length token)))
          (multiple-value-bind (completion success object nmatches possibilities)
              ;; NICKS are bare strings, so identity keys; the default keys
              ;; expect each possibility to be a (name value) list.
              (complete-from-possibilities token nicks '() :action action
                                                          :name-key #'identity
                                                          :value-key #'identity)
            (if completion
                (let* ((unique (and success (eql nmatches 1)))
                       (suffix (cond ((not unique) "")
                                     ((zerop (length prefix)) ": ")
                                     (t " ")))
                       (full (concatenate 'string prefix completion suffix)))
                  (values full success object nmatches possibilities))
                (values so nil nil 0 nil)))
          (values so nil nil 0 nil)))))

;; complete-input must run inside accept's input-editing/rescan environment,
;; or activation never sets a result and the line comes back empty.  So we read
;; the chat line as a CHAT-LINE presentation whose accept method drives
;; complete-input, mirroring the supported NICK accept pattern above.
(define-presentation-type chat-line ())

(define-presentation-method accept ((type chat-line) stream (view textual-view) &key)
  (let ((frame *application-frame*))
    (multiple-value-bind (object success string)
        (complete-input stream
                        (lambda (so action)
                          (complete-chat-token so action frame))
                        :allow-any-input t
                        ;; Space is a normal chat character, not a completer;
                        ;; only TAB triggers completion.
                        :partial-completers '())
      (declare (ignore object success))
      (values (or string "") 'chat-line))))

(defun read-chat-line (stream)
  "Read one whole line of input from STREAM, spaces included, with TAB nick
completion on the last token."
  (with-delimiter-gestures (nil :override t)
    (handler-case
        (accept 'chat-line :stream stream :prompt nil)
      (error () ""))))

(defun %split-first (string)
  "Split STRING on its first run of spaces.  Return (values first rest)."
  (let* ((s (string-left-trim '(#\Space #\Tab) string))
         (sp (position #\Space s)))
    (if sp
        (values (subseq s 0 sp) (string-left-trim '(#\Space) (subseq s sp)))
        (values s ""))))

(defun parse-slash-command (frame rest)
  "Turn a slash command body REST (everything after the /) into a command."
  (multiple-value-bind (word args) (%split-first rest)
    (let ((cmd (string-downcase word))
          (current (let ((b (app-current frame))) (and b (buffer-name b)))))
      (flet ((arg1 () (nth-value 0 (%split-first args))))
        (cond
          ((string= cmd "join")       (list 'com-join (arg1)))
          ((string= cmd "part")       (list 'com-part (if (plusp (length (arg1))) (arg1) current)))
          ((string= cmd "msg")        (multiple-value-bind (tgt text) (%split-first args)
                                        (list 'com-msg tgt text)))
          ((string= cmd "whois")      (list 'com-whois (arg1)))
          ((string= cmd "query")      (list 'com-query (arg1)))
          ((string= cmd "nick")       (list 'com-nick (arg1)))
          ((string= cmd "me")         (list 'com-me args))
          ((string= cmd "quit")       (list 'com-quit))
          ((string= cmd "connect")    (list 'com-connect))
          ((string= cmd "disconnect") (list 'com-disconnect))
          (t (list 'com-note (format nil "unknown command: /~A" word))))))))

(defmethod read-frame-command ((frame clatter-clim) &key (stream *standard-input*))
  ;; Establish a command input context so clicking a presentation (a buffer in
  ;; the list, a nick) still fires its to-command translator.  If no click
  ;; happens, the body reads a free-form line of text instead.
  (with-input-context ('command :override nil) (object)
      (let ((line (string-left-trim '(#\Space #\Tab) (or (read-chat-line stream) ""))))
        (cond
          ((zerop (length line)) nil)
          ((char= (char line 0) #\/) (parse-slash-command frame (subseq line 1)))
          (t (list 'com-say line))))
    (command object)))
