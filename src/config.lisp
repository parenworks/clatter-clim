;;;; config.lisp - persisted user configuration
;;;;
;;;; A small sexp config at ~/.config/clatter-clim/config.lisp, in the same
;;;; spirit as quaestor's.  It seeds the connection defaults and, most
;;;; importantly, remembers the autojoin channels so you do not re-join the
;;;; same channels every session.  Membership changes you make (join/part
;;;; yourself) are written straight back.

(in-package #:clatter-clim)

;;; Connection defaults.  parensmith is the throwaway test nick so a test
;;; connect never collides with the glenneth bouncer session.  LOAD-CONFIG
;;; overwrites these from the config file at startup; the connect command and
;;; dialog read them as their argument defaults.
(defvar *default-server* "irc.libera.chat")
(defvar *default-port* 6697)
(defvar *default-nick* "parensmith")

(defvar *config-path*
  (merge-pathnames ".config/clatter-clim/config.lisp" (user-homedir-pathname))
  "Where the persisted configuration lives.")

(defstruct config
  (server "irc.libera.chat")
  (port 6697)
  (nick "parensmith")
  (autojoin '()))

(defvar *config* (make-config)
  "The active configuration.  Replaced by LOAD-CONFIG at startup.")

(defun load-config (&optional (path *config-path*))
  "Read PATH (a plist sexp) into *CONFIG* if it exists, else keep defaults.
Then reflect server/port/nick into the connect defaults.  Returns *CONFIG*."
  (when (probe-file path)
    (let ((plist (with-open-file (s path :direction :input :if-does-not-exist nil)
                   (read s nil nil))))
      (when (and plist (listp plist))
        (setf *config*
              (make-config :server (or (getf plist :server) "irc.libera.chat")
                           :port (or (getf plist :port) 6697)
                           :nick (or (getf plist :nick) "parensmith")
                           :autojoin (getf plist :autojoin))))))
  (setf *default-server* (config-server *config*)
        *default-port* (config-port *config*)
        *default-nick* (config-nick *config*))
  *config*)

(defun save-config (&optional (path *config-path*))
  "Write *CONFIG* to PATH as a readable plist, creating directories as needed."
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-case* :downcase))
      (prin1 (list :server (config-server *config*)
                   :port (config-port *config*)
                   :nick (config-nick *config*)
                   :autojoin (config-autojoin *config*))
             s)))
  path)

(defun config-add-autojoin (channel)
  "Remember CHANNEL for autojoin and persist.  No-op if already present."
  (unless (member channel (config-autojoin *config*) :test #'string-equal)
    (setf (config-autojoin *config*)
          (append (config-autojoin *config*) (list channel)))
    (save-config)))

(defun config-remove-autojoin (channel)
  "Forget CHANNEL for autojoin and persist."
  (when (member channel (config-autojoin *config*) :test #'string-equal)
    (setf (config-autojoin *config*)
          (remove channel (config-autojoin *config*) :test #'string-equal))
    (save-config)))
