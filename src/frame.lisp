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
(defparameter *colour-channel*    (make-rgb-color 0.45 0.80 0.95)
  "Ink for clickable #channel mentions inside message text.")
(defparameter *colour-url*        (make-rgb-color 0.55 0.85 0.65)
  "Ink for clickable URLs inside message text.")

;;; Custom-drawn buttons (toolbar and dialogs).  These are presentations, not
;;; gadgets, so we own their shape, padding and hover entirely.
(defparameter *btn-bg*       (make-rgb-color 0.18 0.20 0.25))
(defparameter *btn-bg-hover* (make-rgb-color 0.24 0.34 0.50))
(defparameter *btn-fg*       *colour-fg-default*)
(defparameter *btn-radius* 6)
(defparameter *btn-pad-x* 14 "Horizontal padding between a button's text and its edge.")
(defparameter *btn-height* 26)

(defstruct (ui-button (:constructor make-ui-button (label action)))
  "A clickable button: LABEL is drawn, ACTION is a thunk run on click."
  (label "" :type string)
  (action (constantly nil) :type function))

(defun draw-ui-button (stream button x y &key (background *btn-bg*) (foreground *btn-fg*))
  "Draw BUTTON as a rounded, padded box with its top-left at (X, Y), wrapped in
a UI-BUTTON presentation so a translator can fire on click.  Returns the width
drawn so callers can lay buttons out in a row."
  (let* ((label (ui-button-label button))
         (w (+ (text-size stream label) (* 2 *btn-pad-x*)))
         (h *btn-height*))
    ;; :SINGLE-BOX T makes McCLIM highlight the whole button via our
    ;; HIGHLIGHT-PRESENTATION method, rather than drawing default boxes around
    ;; the child rectangle and text records.
    (with-output-as-presentation (stream button 'ui-button :single-box t)
      (clime:draw-rounded-rectangle* stream x y (+ x w) (+ y h)
                                     :radius *btn-radius* :filled t :ink background)
      (draw-text* stream label (+ x (/ w 2)) (+ y (/ h 2))
                  :align-x :center :align-y :center :ink foreground))
    w))

(define-presentation-method highlight-presentation
    ((type ui-button) record stream state)
  ;; On hover, repaint the button with the brighter background; un-highlight is
  ;; handled by McCLIM repainting the region, which redraws the normal button.
  (when (eql state :highlight)
    (let ((button (presentation-object record)))
      (with-bounding-rectangle* (x1 y1 x2 y2) record
        (clime:draw-rounded-rectangle* stream x1 y1 x2 y2
                                       :radius *btn-radius* :filled t :ink *btn-bg-hover*)
        (draw-text* stream (ui-button-label button)
                    (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)
                    :align-x :center :align-y :center :ink *btn-fg*)))))

;;; Flat, modern scrollbars.  McCLIM's default scroll-bar-pane draws a 3D Motif
;;; trough with arrow buttons; we replace its HANDLE-REPAINT with a subtle
;;; trough and a thin rounded thumb (brighter while dragging).  This overrides
;;; the method for every scroll bar in the image, which is what we want.
(defparameter *scrollbar-trough*       (make-rgb-color 0.11 0.12 0.14))
(defparameter *scrollbar-thumb*        (make-rgb-color 0.30 0.32 0.38))
(defparameter *scrollbar-thumb-active* *btn-bg-hover*)

(defmethod handle-repaint ((pane climi::scroll-bar-pane) region)
  (declare (ignore region))
  (let ((trans (climi::scroll-bar-transformation pane)))
    (with-drawing-options (pane :transformation trans)
      ;; The thumb geometry helpers all work in this transformed ("always
      ;; vertical") space, so we draw in it too.
      (with-bounding-rectangle* (tx1 ty1 tx2 ty2)
          (transform-region trans (climi::pane-inner-region pane))
        (draw-rectangle* pane tx1 ty1 tx2 ty2 :ink *scrollbar-trough*)
        (with-bounding-rectangle* (x1 y1 x2 y2) (climi::scroll-bar-thumb-region pane)
          (declare (ignore x1 x2))
          (let ((inset  (max 2 (floor (- tx2 tx1) 4)))
                (active (eq (slot-value pane 'climi::event-state) :dragging)))
            (clime:draw-rounded-rectangle* pane
                                           (+ tx1 inset) (+ y1 1)
                                           (- tx2 inset) (- y2 1)
                                           :radius 4 :filled t
                                           :ink (if active
                                                    *scrollbar-thumb-active*
                                                    *scrollbar-thumb*))))))))

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

(defun channel-token-p (token)
  "True if TOKEN looks like an IRC channel mention (a channel-prefix char
followed by at least one more character)."
  (and (>= (length token) 2)
       (member (char token 0) '(#\# #\& #\+ #\!))))

(defun url-token-p (token)
  "True if TOKEN looks like an http or https URL."
  (let ((n (length token)))
    (or (and (>= n 7) (string-equal "http://" token :end2 7))
        (and (>= n 8) (string-equal "https://" token :end2 8)))))

(defun split-trailing-punctuation (token)
  "Return (values CORE TAIL) splitting trailing sentence punctuation off TOKEN,
so it is not swallowed into a URL or channel name.  At least one character is
always kept in CORE."
  (let ((core-end (length token)))
    (loop while (and (> core-end 1)
                     (find (char token (1- core-end)) ".,!?:;)]}\"'"))
          do (decf core-end))
    (values (subseq token 0 core-end) (subseq token core-end))))

(defun present-token (pane token ptype ink default-ink)
  "Present TOKEN as PTYPE in INK, writing any trailing punctuation as plain
text in DEFAULT-INK."
  (multiple-value-bind (core tail) (split-trailing-punctuation token)
    (with-drawing-options (pane :ink ink)
      (present core ptype :stream pane))
    (when (plusp (length tail))
      (with-drawing-options (pane :ink default-ink)
        (write-string tail pane)))))

(defun present-message-text (pane text default-ink)
  "Write TEXT to PANE, presenting URLs and #channel tokens as clickable
presentations (opened / joined on click) and the rest as plain text in
DEFAULT-INK.  Whitespace runs are emitted verbatim so spacing is preserved."
  (let ((start 0) (len (length text)))
    (flet ((ws-p (c) (member c '(#\Space #\Tab))))
      (loop while (< start len) do
        ;; Emit any run of whitespace verbatim.
        (let ((ws-end start))
          (loop while (and (< ws-end len) (ws-p (char text ws-end))) do (incf ws-end))
          (when (> ws-end start)
            (with-drawing-options (pane :ink default-ink)
              (write-string text pane :start start :end ws-end))
            (setf start ws-end)))
        (when (>= start len) (return))
        ;; Consume the next whitespace-delimited token.
        (let ((tok-end start))
          (loop while (and (< tok-end len) (not (ws-p (char text tok-end)))) do (incf tok-end))
          (let ((token (subseq text start tok-end)))
            (cond
              ((url-token-p token)
               (present-token pane token 'url *colour-url* default-ink))
              ((channel-token-p token)
               (present-token pane token 'irc-channel *colour-channel* default-ink))
              (t
               (with-drawing-options (pane :ink default-ink)
                 (write-string token pane)))))
          (setf start tok-end))))))

(define-application-frame clatter-clim ()
  ((networks     :initform '() :accessor app-networks
                 :documentation "All open server connections (NETWORK objects).")
   (buffers      :initform '() :accessor app-buffers)
   (current      :initform nil :accessor app-current)
   ;; The mailbox is the only state the IRC reader thread writes to, under
   ;; MAILBOX-LOCK.  COM-DRAIN empties it on the frame thread.
   (mailbox      :initform '() :accessor app-mailbox)
   (mailbox-lock :initform (bt:make-lock "clatter-clim-mailbox")
                 :reader app-mailbox-lock)
   ;; Client-side ignore list: lowercased bare nicks whose messages are
   ;; dropped before they reach a buffer.  Mutated only on the frame thread.
   (ignored      :initform '() :accessor app-ignored))
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
                :scroll-bars :vertical
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
   ;; A custom-drawn toolbar: DISPLAY-TOOLBAR paints UI-BUTTON presentations,
   ;; and the TOOLBAR-CLICK translator (commands.lisp) runs the clicked
   ;; button's action.  Clicks are caught by the same input context that
   ;; handles nick/buffer clicks, so no gadget callbacks are needed.
   (toolbar     :application
                :display-function 'display-toolbar
                :scroll-bars nil
                :height 38 :min-height 38 :max-height 38
                :foreground *colour-fg-default*
                :background *colour-bg-main*))
  (:layouts
   (default
    (vertically ()
      toolbar
      (horizontally ()
        (1/6 buffer-list)
        (2/3 (vertically ()
               status
               messages))
        (1/6 nick-list))
      input))))

(defmethod (setf app-current) :after (buffer (frame clatter-clim))
  "Selecting a buffer clears its unseen-activity markers."
  (when buffer
    (setf (buffer-unread buffer) 0
          (buffer-ping buffer) nil)))

(defun current-network (frame)
  "The NETWORK of the current buffer, or NIL when nothing is open."
  (let ((b (app-current frame)))
    (and b (buffer-network b))))

(defun app-connection (frame)
  "The clatter-irc connection commands should act on: the current buffer's
network connection.  NIL when disconnected or no buffer is open.  Replaces the
old single-connection slot now that a frame may hold several networks."
  (let ((n (current-network frame)))
    (and n (network-connection n))))

(defun hhmm (universal)
  "Format a universal time as a local HH:MM string."
  (multiple-value-bind (s m h) (decode-universal-time (or universal (get-universal-time)))
    (declare (ignore s))
    (format nil "~2,'0D:~2,'0D" h m)))

(defun format-log-line (line)
  "Render an IRC-LINE as one timestamped text line for the on-disk log."
  (multiple-value-bind (s m h d mo y)
      (decode-universal-time (or (irc-line-time line) (get-universal-time)))
    (let ((ts (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y mo d h m s))
          (nick (irc-line-nick line))
          (text (irc-line-text line)))
      (ecase (irc-line-kind line)
        (:privmsg (format nil "~A <~A> ~A" ts nick text))
        (:notice  (format nil "~A -~A- ~A" ts nick text))
        ((:join :part :quit :topic :system :dcc-offer) (format nil "~A -!- ~A" ts text))))))

(defun log-line (frame network target line)
  "Append LINE to TARGET's on-disk log under NETWORK's server directory.
Best-effort: errors (a full disk, a bad path) are swallowed so logging never
disturbs the UI."
  (declare (ignore frame))
  (ignore-errors
    (let* ((server (or (and network (network-label network)) "server"))
           (path (buffer-log-path server target)))
      (ensure-directories-exist path)
      (with-open-file (s path :direction :output
                              :if-exists :append :if-does-not-exist :create
                              :external-format :utf-8)
        (write-line (format-log-line line) s)))))

(defun note-activity (frame buffer line)
  "Mark BUFFER as having unseen activity when it is not the current buffer, so
the buffer list can show an unread count and a ping marker.  Only message
lines (privmsg/notice) count; join/part churn does not."
  (when (and (not (eq buffer (app-current frame)))
             (member (irc-line-kind line) '(:privmsg :notice)))
    (incf (buffer-unread buffer))
    (let ((me (let* ((net (buffer-network buffer))
                     (conn (and net (network-connection net))))
                (and conn (irc:connection-nick conn)))))
      (when (and me (mentions-p (irc-line-text line) me))
        (setf (buffer-ping buffer) t)))))

;;; ----------------------------------------------------------------------
;;; Display functions.  Each reads the model and paints one pane.  They are
;;; backend-agnostic: identical code drives CLX, the terminal and the browser.
;;; ----------------------------------------------------------------------

(defun display-buffer-line (frame pane b)
  "Paint one buffer row B as a clickable BUFFER presentation."
  (let* ((current-p (eq b (app-current frame)))
         (unread (buffer-unread b))
         (ping (buffer-ping b))
         (ink (cond (current-p *colour-heading*)
                    (ping *colour-highlight*)
                    ((plusp unread) *colour-fg-default*)
                    (t *colour-muted*)))
         (badge (with-output-to-string (s)
                  (when (plusp unread) (format s " (~D)" unread))
                  (when ping (write-string " *" s)))))
    (with-output-as-presentation (pane b 'buffer)
      (with-drawing-options (pane :ink ink)
        (format pane "~:[  ~;> ~]~A~A~%" current-p (buffer-name b) badge)))))

(defun display-buffer-list (frame pane)
  ;; Group buffers under their network.  With one network the header is
  ;; suppressed so the single-server case looks exactly as before; with several
  ;; networks each gets a labelled section, keeping same-named channels apart.
  (let ((networks (app-networks frame)))
    (if (cdr networks)
        (dolist (net networks)
          (with-drawing-options (pane :ink *colour-heading*)
            (format pane "~A~%" (network-label net)))
          (dolist (b (app-buffers frame))
            (when (eq (buffer-network b) net)
              (display-buffer-line frame pane b))))
        (dolist (b (app-buffers frame))
          (display-buffer-line frame pane b)))))

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
             (with-drawing-options (pane :ink *colour-muted*)
               (format pane "~A " (hhmm (irc-line-time line))))
             (write-string "<" pane)
             (with-drawing-options (pane :ink (nick-ink nick))
               (present nick 'nick :stream pane))
             (write-string "> " pane)
             (present-message-text pane text
                                   (if ping *colour-highlight* *colour-fg-default*))
             (terpri pane)))
          ((:join :part :quit :topic :system)
           (with-drawing-options (pane :ink *colour-muted*)
             (format pane "~A -!- ~A~%" (hhmm (irc-line-time line)) (irc-line-text line))))
          (:dcc-offer
           ;; A clickable incoming DCC offer: the description, then an [accept]
           ;; affordance presented as a DCC-OFFER (left-click accepts, the
           ;; right-click menu rejects; see the translators in commands.lisp).
           (with-drawing-options (pane :ink *colour-muted*)
             (format pane "~A -!- " (hhmm (irc-line-time line))))
           (with-drawing-options (pane :ink *colour-highlight*)
             (write-string (irc-line-text line) pane))
           (write-string " " pane)
           (with-output-as-presentation (pane (irc-line-data line) 'dcc-offer)
             (with-drawing-options (pane :ink *colour-url*)
               (write-string "[accept]" pane)))
           (terpri pane)))))))

(defun display-nick-list (frame pane)
  (let ((b (app-current frame)))
    (when (and b (eq (buffer-kind b) :channel))
      (dolist (n (buffer-users b))
        (with-drawing-options (pane :ink (nick-ink n))
          (present n 'nick :stream pane))
        (terpri pane)))))

(defun display-status (frame pane)
  (let* ((net (current-network frame))
         (conn (and net (network-connection net)))
         (b (app-current frame))
         (connected (and conn (irc:connectedp conn))))
    (format pane "~A | ~A | ~A | ~A"
            (if connected (irc:connection-nick conn) "(disconnected)")
            (if net (network-label net) "(no network)")
            (if b (buffer-name b) "(no buffer)")
            (if b (buffer-topic b) ""))))

(defun display-toolbar (frame pane)
  "Paint the toolbar buttons in a left-aligned row."
  (declare (ignore frame))
  (let ((x 8) (y 6) (gap 8))
    (dolist (button (list (make-ui-button "Connect"    (lambda () (com-connect)))
                          (make-ui-button "Disconnect" (lambda () (com-disconnect)))
                          (make-ui-button "Configure"  (lambda () (com-configure)))))
      (incf x (+ (draw-ui-button pane button x y) gap)))))

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
          ((string= cmd "dcc")        (parse-dcc-command args))
          (t (list 'com-note (format nil "unknown command: /~A" word))))))))

(defun parse-dcc-command (args)
  "Turn the body of a /dcc command into a command list:
/dcc chat <nick> | send <nick> <file> | accept <id> | reject <id> | list."
  (multiple-value-bind (sub rest) (%split-first args)
    (let ((sub (string-downcase sub)))
      (flet ((int (s) (parse-integer s :junk-allowed t)))
        (cond
          ((string= sub "chat")   (list 'com-dcc-chat (nth-value 0 (%split-first rest))))
          ((string= sub "send")   (multiple-value-bind (n f) (%split-first rest)
                                    (list 'com-dcc-send n f)))
          ((string= sub "accept") (let ((id (int (nth-value 0 (%split-first rest)))))
                                    (if id (list 'com-dcc-accept-id id)
                                        (list 'com-note "usage: /dcc accept <id>"))))
          ((string= sub "reject") (let ((id (int (nth-value 0 (%split-first rest)))))
                                    (if id (list 'com-dcc-reject-id id)
                                        (list 'com-note "usage: /dcc reject <id>"))))
          ((string= sub "list")   (list 'com-dcc-list))
          (t (list 'com-note "usage: /dcc chat|send|accept|reject|list")))))))

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

;;; ----------------------------------------------------------------------
;;; Connect / config dialog.
;;;
;;; A small themed application frame rather than ACCEPTING-VALUES: the latter's
;;; own-window is stubbornly light and cannot be recoloured, and its INTEGER
;;; field is not an editable box.  Here every pane carries the dark palette and
;;; Port is a real text field.
;;; ----------------------------------------------------------------------

(defun split-channels (string)
  "Split STRING on whitespace or commas into a list of non-empty channel names."
  (flet ((sep-p (c) (member c '(#\Space #\Tab #\Newline #\Return #\,))))
    (let ((result '()) (start nil) (len (length string)))
      (dotimes (i len)
        (if (sep-p (char string i))
            (when start (push (subseq string start i) result) (setf start nil))
            (unless start (setf start i))))
      (when start (push (subseq string start) result))
      (nreverse result))))

(defun config-dialog-label (text)
  "A right-padded label pane in the dialog's palette."
  (make-pane 'label-pane :label text
                         :align-x :right
                         :foreground *colour-fg-default*
                         :background *colour-bg-main*))

(defun config-dialog-commit (acceptedp)
  "Action thunk for the dialog's OK/Cancel UI-BUTTONs.  Runs inside
COM-INVOKE-BUTTON, so *APPLICATION-FRAME* is the dialog.  On accept, snapshot
the field values into the frame's RESULT slot; either way exit the dialog."
  (let ((frame *application-frame*))
    (when acceptedp
      (flet ((val (name) (gadget-value (find-pane-named frame name))))
        (setf (config-dialog-result frame)
              (list :server (val 'server-field)
                    :port (val 'port-field)
                    :nick (val 'nick-field)
                    :password (val 'pass-field)
                    :autojoin (val 'autojoin-field)))))
    (frame-exit frame)))

(defun display-config-buttons (frame pane)
  "Paint the dialog's OK/Cancel buttons as custom UI-BUTTONs."
  (declare (ignore frame))
  (let ((x 8) (y 7) (gap 8))
    (dolist (button (list (make-ui-button "OK"     (lambda () (config-dialog-commit t)))
                          (make-ui-button "Cancel" (lambda () (config-dialog-commit nil)))))
      (incf x (+ (draw-ui-button pane button x y) gap)))))

(define-application-frame config-dialog ()
  ((result :initform nil :accessor config-dialog-result))
  (:menu-bar nil)
  ;; Inherit the main command table so the UI-BUTTON-CLICK translator and
  ;; COM-INVOKE-BUTTON apply here, letting OK/Cancel be custom presentations.
  (:command-table (config-dialog :inherit-from (clatter-clim)))
  (:panes
   (server-field   (make-pane 'text-field :value (config-server *config*)
                                           :foreground *colour-fg-default*
                                           :background *colour-bg-accent*))
   (port-field     (make-pane 'text-field
                              :value (princ-to-string (config-port *config*))
                              :foreground *colour-fg-default*
                              :background *colour-bg-accent*))
   (nick-field     (make-pane 'text-field :value (config-nick *config*)
                                           :foreground *colour-fg-default*
                                           :background *colour-bg-accent*))
   (pass-field     (make-pane 'text-field :value (config-sasl-password *config*)
                                           :foreground *colour-fg-default*
                                           :background *colour-bg-accent*))
   (autojoin-field (make-pane 'text-editor
                              :value (format nil "~{~A~^ ~}" (config-autojoin *config*))
                              :ncolumns 40 :nlines 3
                              :foreground *colour-fg-default*
                              :background *colour-bg-accent*))
   (buttons        :application
                   :display-function 'display-config-buttons
                   :scroll-bars nil
                   :height 40 :min-height 40 :max-height 40
                   :foreground *colour-fg-default*
                   :background *colour-bg-main*))
  (:layouts
   (default
    (vertically (:spacing 10 :background *colour-bg-main*)
      (tabling (:spacing 8 :background *colour-bg-main*)
        (list (config-dialog-label "Server")            server-field)
        (list (config-dialog-label "Port")              port-field)
        (list (config-dialog-label "Nick")              nick-field)
        (list (config-dialog-label "SASL password")     pass-field)
        (list (config-dialog-label "Autojoin channels") autojoin-field))
      buttons))))

(defun run-config-dialog ()
  "Open the themed config dialog modally on the calling frame's port.  Returns
the result plist on OK, or NIL if the user cancelled."
  (let ((frame (make-application-frame 'config-dialog
                                       :frame-manager (frame-manager *application-frame*)
                                       :pretty-name "Connect / Configure"
                                       :width 560 :height 320)))
    (run-frame-top-level frame)
    (config-dialog-result frame)))
