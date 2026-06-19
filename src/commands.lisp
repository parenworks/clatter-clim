;;;; commands.lisp - CLIM commands and presentation translators
;;;;
;;;; All commands run on the frame thread.  Outbound IRC (privmsg, join, ...)
;;;; goes straight to clatter-irc, which is thread-safe for sending.  Inbound
;;;; IRC arrives through the bridge (bridge.lisp), never here.

(in-package #:clatter-clim)

;;; ----------------------------------------------------------------------
;;; Connection lifecycle
;;; ----------------------------------------------------------------------

(define-clatter-clim-command (com-connect :name "Connect")
    (&key (server 'string :default *default-server*)
          (nick 'string :default *default-nick*)
          ;; A non-empty password authenticates to services via SASL PLAIN
          ;; during registration, which is the preferred NickServ identify
          ;; path (no plaintext IDENTIFY on the wire).
          (password 'string :default ""))
  (let* ((frame *application-frame*)
         (use-sasl (plusp (length password)))
         (conn (irc:make-connection server nick :tls t
                                    :sasl-username (when use-sasl nick)
                                    :sasl-password (when use-sasl password))))
    (setf (app-connection frame) conn)
    (ensure-buffer frame server :server)
    (install-irc-hooks frame conn)
    ;; clatter-irc spawns its own reader thread; do the connect off the UI
    ;; thread so a slow TLS handshake never freezes the frame.
    (clim-sys:make-process (lambda () (irc:connect conn))
                           :name "clatter-irc-connect")
    (redisplay-current frame)))

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
