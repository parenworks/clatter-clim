;;;; commands.lisp - CLIM commands and presentation translators
;;;;
;;;; All commands run on the frame thread.  Outbound IRC (privmsg, join, ...)
;;;; goes straight to clatter-irc, which is thread-safe for sending.  Inbound
;;;; IRC arrives through the bridge (bridge.lisp), never here.

(in-package #:clatter-clim)

;;; ----------------------------------------------------------------------
;;; Connection lifecycle
;;; ----------------------------------------------------------------------

(defun start-connection (frame server nick password)
  "Open an IRC connection for FRAME to SERVER as NICK, authenticating via SASL
PLAIN when PASSWORD is non-empty.  The connect runs off the UI thread so a slow
TLS handshake never freezes the frame."
  (let* ((use-sasl (plusp (length password)))
         (conn (irc:make-connection server nick :tls t
                                    :sasl-username (when use-sasl nick)
                                    :sasl-password (when use-sasl password))))
    (setf (app-connection frame) conn)
    (ensure-buffer frame server :server)
    (install-irc-hooks frame conn)
    (clim-sys:make-process (lambda () (irc:connect conn))
                           :name "clatter-irc-connect")
    (redisplay-current frame)
    conn))

(define-clatter-clim-command (com-connect :name "Connect")
    (&key (server 'string :default *default-server*)
          (nick 'string :default *default-nick*)
          ;; A non-empty password authenticates to services via SASL PLAIN
          ;; during registration, which is the preferred NickServ identify
          ;; path (no plaintext IDENTIFY on the wire).
          (password 'string :default *default-sasl-password*))
  (start-connection *application-frame* server nick password))

;;; A connect/config dialog.  Edits the persisted config in place, saves it,
;;; and connects with the chosen values.  Cancel leaves everything untouched.
(define-clatter-clim-command (com-configure :name "Configure")
    ()
  (let* ((frame *application-frame*)
         (result (run-config-dialog)))
    (when result
      (destructuring-bind (&key server port nick password autojoin) result
        (let ((server   (string-trim '(#\Space #\Tab) server))
              (nick     (string-trim '(#\Space #\Tab) nick))
              ;; PORT comes back as a string from the text field; keep the
              ;; current value if it is not a valid integer.
              (port     (or (parse-integer port :junk-allowed t) (config-port *config*)))
              (channels (split-channels autojoin)))
          (setf (config-server *config*) server
                (config-port *config*) port
                (config-nick *config*) nick
                (config-sasl-password *config*) password
                (config-autojoin *config*) channels
                *default-server* server
                *default-port* port
                *default-nick* nick
                *default-sasl-password* password)
          (save-config)
          (start-connection frame server nick password))))))

(define-clatter-clim-command (com-disconnect :name "Disconnect")
    (&key (message 'string :default "clatter-clim"))
  (let* ((frame *application-frame*) (conn (app-connection frame)))
    (when conn
      ;; Turn off auto-reconnect first, or the reader thread reconnects a few
      ;; seconds after we close the socket.
      (setf (irc:connection-reconnect-enabled conn) nil)
      (ignore-errors (irc:disconnect conn message)))
    (redisplay-current frame)))

(define-clatter-clim-command (com-quit :name "Quit")
    (&key (message 'string :default "clatter-clim"))
  (let* ((frame *application-frame*) (conn (app-connection frame)))
    (when conn
      (setf (irc:connection-reconnect-enabled conn) nil)
      (ignore-errors (irc:quit conn message)))
    (frame-exit frame)))

;;; Send a PRIVMSG to any target.  This is the path to services such as
;;; NickServ and ChanServ, for example: Msg NickServ "identify secret".
(define-clatter-clim-command (com-msg :name "Msg")
    ((target 'string) (text 'string))
  (let ((conn (app-connection *application-frame*)))
    (when conn (irc:privmsg conn target text))))

;;; ----------------------------------------------------------------------
;;; Channel and conversation commands
;;; ----------------------------------------------------------------------

(define-clatter-clim-command (com-join :name "Join")
    ((channel 'irc-channel))
  (let* ((frame *application-frame*) (conn (app-connection frame)))
    (when conn
      (irc:join conn channel)
      (setf (app-current frame) (ensure-buffer frame channel :channel))
      (redisplay-current frame))))

(define-clatter-clim-command (com-part :name "Part")
    ((channel 'irc-channel))
  (let ((conn (app-connection *application-frame*)))
    (when conn (irc:part conn channel))))

(define-clatter-clim-command (com-whois :name "Whois")
    ((who 'nick))
  (let ((conn (app-connection *application-frame*)))
    (when conn (irc:whois conn who))))

(define-clatter-clim-command (com-query :name "Query")
    ((who 'nick))
  (let ((frame *application-frame*))
    (setf (app-current frame) (ensure-buffer frame who :query))
    (redisplay-current frame)))

(define-clatter-clim-command (com-switch-buffer :name "Switch Buffer")
    ((buffer 'buffer))
  (let ((frame *application-frame*))
    (setf (app-current frame) buffer)
    (redisplay-current frame)))

(define-clatter-clim-command (com-say :name "Say")
    ((text 'string))
  (let* ((frame *application-frame*)
         (conn (app-connection frame))
         (b (app-current frame)))
    (when (and conn b (buffer-target-p b))
      (irc:privmsg conn (buffer-name b) text)
      ;; With echo-message active, the server echoes our own PRIVMSG back and
      ;; on-privmsg renders it; echoing locally too would double it.  Only
      ;; echo locally when the server will not.
      (unless (irc:cap-enabled-p conn "echo-message")
        (buffer-add-line b (make-irc-line :privmsg (irc:connection-nick conn) text))
        (redisplay-current frame)))))

(define-clatter-clim-command (com-nick :name "Nick")
    ((new-nick 'string))
  (let ((conn (app-connection *application-frame*)))
    (when conn (irc:nick conn new-nick))))

(define-clatter-clim-command (com-me :name "Me")
    ((action 'string))
  (let* ((frame *application-frame*)
         (conn (app-connection frame))
         (b (app-current frame)))
    (when (and conn b (buffer-target-p b))
      (irc:ctcp conn (buffer-name b) "ACTION" action)
      (unless (irc:cap-enabled-p conn "echo-message")
        (buffer-add-line b (make-irc-line :privmsg (irc:connection-nick conn)
                                          (format nil "* ~A ~A"
                                                  (irc:connection-nick conn) action)))
        (redisplay-current frame)))))

;;; Internal: print a client-side note into the current buffer (no name, not
;;; user-invokable).  Used by the slash-command parser for unknown commands.
(define-clatter-clim-command (com-note :name nil)
    ((text 'string))
  (let* ((frame *application-frame*) (b (app-current frame)))
    (when b
      (buffer-add-line b (make-irc-line :system nil text))
      (redisplay-current frame))))

;;; ----------------------------------------------------------------------
;;; Presentation translators: click an IRC noun, run a command
;;; ----------------------------------------------------------------------

(define-presentation-to-command-translator nick-to-whois
    (nick com-whois clatter-clim :gesture :select :documentation "Whois")
    (object) (list object))

(define-presentation-to-command-translator nick-to-query
    (nick com-query clatter-clim :gesture :describe :documentation "Open query")
    (object) (list object))

(define-presentation-to-command-translator channel-to-join
    (irc-channel com-join clatter-clim :gesture :select :documentation "Join")
    (object) (list object))

(define-presentation-to-command-translator buffer-to-switch
    (buffer com-switch-buffer clatter-clim :gesture :select :documentation "Switch")
    (object) (list object))

;;; Custom-drawn buttons (toolbar, dialogs): clicking a UI-BUTTON runs its
;;; stored action thunk.  This command has no :name so it stays out of menus.
(define-clatter-clim-command (com-invoke-button)
    ((button 'ui-button))
  (funcall (ui-button-action button)))

(define-presentation-to-command-translator ui-button-click
    (ui-button com-invoke-button clatter-clim :gesture :select :documentation "Activate")
    (object) (list object))
