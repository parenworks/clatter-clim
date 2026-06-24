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
PLAIN when PASSWORD is non-empty.  Each call adds a new NETWORK, so connecting
to a second server simply opens another set of buffers in the same list.  The
connect runs off the UI thread so a slow TLS handshake never freezes the frame."
  (let* ((use-sasl (plusp (length password)))
         (conn (irc:make-connection server nick :tls t
                                    :sasl-username (when use-sasl nick)
                                    :sasl-password (when use-sasl password)))
         (network (make-instance 'network :connection conn :label server)))
    (setf (app-networks frame) (append (app-networks frame) (list network)))
    ;; Switch to the new server buffer so the user sees the network they just
    ;; opened, even when other networks are already present.
    (setf (app-current frame) (ensure-buffer frame network server :server))
    (install-irc-hooks frame network)
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
  ;; Quitting the client closes every open network, not just the current one.
  (let ((frame *application-frame*))
    (dolist (net (app-networks frame))
      (let ((conn (network-connection net)))
        (when conn
          (setf (irc:connection-reconnect-enabled conn) nil)
          (ignore-errors (irc:quit conn message)))))
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
  (let* ((frame *application-frame*)
         (network (current-network frame))
         (conn (app-connection frame)))
    (when conn
      (irc:join conn channel)
      (setf (app-current frame) (ensure-buffer frame network channel :channel))
      (redisplay-current frame))))

(define-clatter-clim-command (com-part :name "Part")
    ((channel 'irc-channel))
  (let ((conn (app-connection *application-frame*)))
    (when conn (irc:part conn channel))))

;;; Open a clicked URL in the system browser.  XDG-OPEN is the Linux launcher;
;;; the call is best-effort so a missing launcher never errors into the UI.
(defun open-url (url)
  "Hand URL to the desktop's default web browser."
  (ignore-errors
    (uiop:launch-program (list "xdg-open" url))))

(define-clatter-clim-command (com-open-url :name "Open URL")
    ((url 'url))
  (open-url url))

(define-clatter-clim-command (com-whois :name "Whois")
    ((who 'nick))
  (let ((conn (app-connection *application-frame*)))
    (when conn (irc:whois conn who))))

(define-clatter-clim-command (com-query :name "Query")
    ((who 'nick))
  (let* ((frame *application-frame*) (network (current-network frame)))
    (when network
      (setf (app-current frame) (ensure-buffer frame network who :query))
      (redisplay-current frame))))

(define-clatter-clim-command (com-switch-buffer :name "Switch Buffer")
    ((buffer 'buffer))
  (let ((frame *application-frame*))
    (setf (app-current frame) buffer)
    (redisplay-current frame)))

(define-clatter-clim-command (com-say :name "Say")
    ((text 'string))
  (let* ((frame *application-frame*)
         (b (app-current frame)))
    (cond
      ;; A DCC chat buffer sends over its direct connection, not the server.
      ;; DCC has no server echo, so we always render our own line locally.
      ((and b (eq (buffer-kind b) :dcc) (buffer-dcc b))
       (ignore-errors (irc:dcc-chat-send (buffer-dcc b) text))
       (let ((me (let ((net (buffer-network b)))
                   (and net (network-connection net)
                        (irc:connection-nick (network-connection net))))))
         (buffer-add-line b (make-irc-line :privmsg (or me "me") text)))
       (redisplay-current frame))
      (t
       (let ((conn (app-connection frame)))
         (when (and conn b (buffer-target-p b))
           (irc:privmsg conn (buffer-name b) text)
           ;; With echo-message active, the server echoes our own PRIVMSG back
           ;; and on-privmsg renders it; echoing locally too would double it.
           ;; Only echo locally when the server will not.
           (unless (irc:cap-enabled-p conn "echo-message")
             (let ((line (make-irc-line :privmsg (irc:connection-nick conn) text)))
               (buffer-add-line b line)
               (log-line frame (buffer-network b) (buffer-name b) line))
             (redisplay-current frame))))))))

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
        (let ((line (make-irc-line :privmsg (irc:connection-nick conn)
                                   (format nil "* ~A ~A"
                                           (irc:connection-nick conn) action))))
          (buffer-add-line b line)
          (log-line frame (buffer-network b) (buffer-name b) line))
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
;;; DCC: direct client-to-client chat and file transfer.  clatter-irc owns the
;;; transport (the dcc-manager and its connection threads); here we surface it
;;; as commands, a clickable offer presentation, and a :dcc chat buffer.
;;; ----------------------------------------------------------------------

(defun network-dcc (network)
  "The DCC manager for NETWORK, created on first use."
  (or (network-dcc-manager network)
      (setf (network-dcc-manager network)
            (irc:make-dcc-manager (network-connection network)))))

(defun dcc-buffer-name (nick)
  "Buffer name for a DCC chat with NICK.  The leading '=' keeps it distinct
from a same-named query or channel buffer (the classic DCC convention)."
  (format nil "=~A" nick))

(defun find-dcc (network id)
  "The DCC connection with numeric ID on NETWORK, or NIL."
  (let ((mgr (network-dcc-manager network)))
    (and mgr (find id (irc:dcc-list mgr) :key #'irc:dcc-connection-id))))

(defun open-dcc-buffer (frame network chat)
  "Ensure and select the :dcc buffer for CHAT's peer, wiring CHAT so its inbound
lines post to that buffer.  Frame thread only."
  (let* ((nick (irc:dcc-connection-nick chat))
         (b (ensure-buffer frame network (dcc-buffer-name nick) :dcc)))
    (setf (buffer-dcc b) chat
          ;; The callback runs on the DCC reader thread, so it only posts to the
          ;; mailbox; the bridge mutates the buffer on the frame thread.
          (irc:dcc-chat-on-message chat)
          (lambda (c line)
            (declare (ignore c))
            (post-update frame (list :dcc-chat-line network chat line))))
    (setf (app-current frame) b)
    b))

(defun %dcc-accept (frame network connection)
  "Accept the pending DCC CONNECTION on NETWORK.  A chat opens its buffer first
so the first inbound line has somewhere to land."
  (when (and network connection)
    (when (typep connection 'irc:dcc-chat)
      (open-dcc-buffer frame network connection))
    (irc:dcc-accept (network-dcc network) (irc:dcc-connection-id connection))
    (com-note (format nil "DCC #~A accepted" (irc:dcc-connection-id connection)))
    (redisplay-current frame)))

(defun %dcc-reject (frame network connection)
  "Reject the pending DCC CONNECTION on NETWORK."
  (declare (ignore frame))
  (when (and network connection)
    (irc:dcc-reject (network-dcc network) (irc:dcc-connection-id connection))
    (com-note (format nil "DCC #~A rejected" (irc:dcc-connection-id connection)))))

(define-clatter-clim-command (com-dcc-chat :name "DCC Chat")
    ((who 'nick))
  (let* ((frame *application-frame*)
         (network (current-network frame))
         (conn (and network (network-connection network))))
    (when conn
      (let ((chat (irc:dcc-initiate-chat (network-dcc network) (bare-nick who) conn)))
        (when chat
          (open-dcc-buffer frame network chat)
          (com-note (format nil "DCC CHAT offered to ~A, waiting for them to connect..."
                            (bare-nick who)))
          (redisplay-current frame))))))

(define-clatter-clim-command (com-dcc-send :name "DCC Send")
    ((who 'nick) (file 'string))
  (let* ((frame *application-frame*)
         (network (current-network frame))
         (conn (and network (network-connection network))))
    (when conn
      (let ((send (irc:dcc-initiate-send (network-dcc network) (bare-nick who) file conn)))
        (com-note (if send
                      (format nil "DCC SEND ~A offered to ~A" file (bare-nick who))
                      (format nil "DCC SEND failed (file not found?): ~A" file)))))))

;; Accept/reject from the clickable offer presentation (object is a DCC-OFFER).
(define-clatter-clim-command (com-dcc-accept :name "DCC Accept")
    ((offer 'dcc-offer))
  (%dcc-accept *application-frame* (dcc-offer-network offer) (dcc-offer-connection offer)))

(define-clatter-clim-command (com-dcc-reject :name "DCC Reject")
    ((offer 'dcc-offer))
  (%dcc-reject *application-frame* (dcc-offer-network offer) (dcc-offer-connection offer)))

;; Accept/reject/list by id, for typed /dcc commands.
(define-clatter-clim-command (com-dcc-accept-id :name "DCC Accept Id")
    ((id 'integer))
  (let* ((frame *application-frame*) (network (current-network frame)))
    (when network (%dcc-accept frame network (find-dcc network id)))))

(define-clatter-clim-command (com-dcc-reject-id :name "DCC Reject Id")
    ((id 'integer))
  (let* ((frame *application-frame*) (network (current-network frame)))
    (when network (%dcc-reject frame network (find-dcc network id)))))

(define-clatter-clim-command (com-dcc-list :name "DCC List")
    ()
  (let* ((frame *application-frame*)
         (network (current-network frame))
         (mgr (and network (network-dcc-manager network)))
         (conns (and mgr (irc:dcc-list mgr))))
    (if conns
        (dolist (c conns)
          (com-note (format nil "DCC #~A ~A ~A (~A)"
                            (irc:dcc-connection-id c)
                            (if (typep c 'irc:dcc-chat) "CHAT" "SEND")
                            (irc:dcc-connection-nick c)
                            (irc:dcc-connection-state c))))
        (com-note "no DCC connections"))))

;;; ----------------------------------------------------------------------
;;; Nick operator / moderation actions (the right-click nick menu).  Each
;;; operates on the current channel and the clicked nick; outside a channel
;;; they are harmless no-ops.
;;; ----------------------------------------------------------------------

(defun bare-nick (nick)
  "NICK with any mode-prefix characters (@%+~&!.) stripped."
  (string-left-trim "@%+~&!." nick))

(defun current-channel-name (frame)
  "The current buffer's name when it is a channel, else NIL."
  (let ((b (app-current frame)))
    (and b (eq (buffer-kind b) :channel) (buffer-name b))))

(defun nick-ignored-p (frame nick)
  "True when NICK (prefixes and case ignored) is on FRAME's ignore list."
  (and nick
       (member (string-downcase (bare-nick nick))
               (app-ignored frame) :test #'string=)
       t))

(defmacro define-nick-mode-command (name title mode-string)
  "Define a channel-mode nick command NAME with menu TITLE that applies
MODE-STRING (e.g. \"+o\") to the clicked nick in the current channel."
  `(define-clatter-clim-command (,name :name ,title)
       ((who 'nick))
     (let* ((frame *application-frame*)
            (conn (app-connection frame))
            (chan (current-channel-name frame)))
       (when (and conn chan)
         (irc:mode conn chan ,mode-string (bare-nick who))))))

(define-nick-mode-command com-op      "Op"       "+o")
(define-nick-mode-command com-deop    "De-op"    "-o")
(define-nick-mode-command com-voice   "Voice"    "+v")
(define-nick-mode-command com-devoice "De-voice" "-v")

(define-clatter-clim-command (com-kick :name "Kick")
    ((who 'nick))
  (let* ((frame *application-frame*)
         (conn (app-connection frame))
         (chan (current-channel-name frame)))
    (when (and conn chan)
      (irc:kick conn chan (bare-nick who)))))

(define-clatter-clim-command (com-ban :name "Ban")
    ((who 'nick))
  (let* ((frame *application-frame*)
         (conn (app-connection frame))
         (chan (current-channel-name frame)))
    (when (and conn chan)
      (irc:mode conn chan "+b" (format nil "~A!*@*" (bare-nick who))))))

(define-clatter-clim-command (com-ignore :name "Ignore")
    ((who 'nick))
  (let* ((frame *application-frame*)
         (bare (string-downcase (bare-nick who)))
         (now-ignoring (not (member bare (app-ignored frame) :test #'string=))))
    (setf (app-ignored frame)
          (if now-ignoring
              (cons bare (app-ignored frame))
              (remove bare (app-ignored frame) :test #'string=)))
    (com-note (format nil "~:[no longer ignoring~;ignoring~] ~A" now-ignoring bare))))

;;; ----------------------------------------------------------------------
;;; Presentation translators: click an IRC noun, run a command
;;; ----------------------------------------------------------------------

(define-presentation-to-command-translator nick-to-whois
    (nick com-whois clatter-clim :gesture :select :documentation "Whois")
    (object) (list object))

(define-presentation-to-command-translator nick-to-query
    (nick com-query clatter-clim :gesture :describe :documentation "Open query")
    (object) (list object))

;;; Right-click a nick for the rest of the actions.  These have :gesture NIL so
;;; they never fire on a plain click, but with :menu T (the default) they all
;;; appear in McCLIM's presentation menu (the global :menu gesture, right
;;; button), alongside Whois and Open query.
(macrolet ((menu-action (name command doc)
             `(define-presentation-to-command-translator ,name
                  (nick ,command clatter-clim :gesture nil :documentation ,doc)
                  (object) (list object))))
  (menu-action nick-to-op      com-op      "Op")
  (menu-action nick-to-deop    com-deop    "De-op")
  (menu-action nick-to-voice   com-voice   "Voice")
  (menu-action nick-to-devoice com-devoice "De-voice")
  (menu-action nick-to-kick     com-kick     "Kick")
  (menu-action nick-to-ban      com-ban      "Ban")
  (menu-action nick-to-ignore   com-ignore   "Ignore / unignore")
  ;; DCC Send needs a file path too; the translator supplies only the nick, so
  ;; CLIM prompts in the interactor for the remaining FILE argument.
  (menu-action nick-to-dcc-chat com-dcc-chat "DCC Chat")
  (menu-action nick-to-dcc-send com-dcc-send "DCC Send"))

(define-presentation-to-command-translator channel-to-join
    (irc-channel com-join clatter-clim :gesture :select :documentation "Join")
    (object) (list object))

(define-presentation-to-command-translator url-to-open
    (url com-open-url clatter-clim :gesture :select :documentation "Open URL")
    (object) (list object))

(define-presentation-to-command-translator buffer-to-switch
    (buffer com-switch-buffer clatter-clim :gesture :select :documentation "Switch")
    (object) (list object))

;;; A clickable incoming DCC offer: left-click accepts, the right-click menu
;;; offers reject (mirroring the nick translators above).
(define-presentation-to-command-translator dcc-offer-accept
    (dcc-offer com-dcc-accept clatter-clim :gesture :select :documentation "Accept DCC")
    (object) (list object))

(define-presentation-to-command-translator dcc-offer-reject
    (dcc-offer com-dcc-reject clatter-clim :gesture nil :documentation "Reject DCC")
    (object) (list object))

;;; Custom-drawn buttons (toolbar, dialogs): clicking a UI-BUTTON runs its
;;; stored action thunk.  This command has no :name so it stays out of menus.
(define-clatter-clim-command (com-invoke-button)
    ((button 'ui-button))
  (funcall (ui-button-action button)))

(define-presentation-to-command-translator ui-button-click
    (ui-button com-invoke-button clatter-clim :gesture :select :documentation "Activate")
    (object) (list object))
