;;;; model.lisp - the IRC view model
;;;;
;;;; Plain data, no CLIM and no clatter-irc here.  The model is mutated
;;;; only on the CLIM frame thread (see bridge.lisp): the IRC reader thread
;;;; never touches it directly, it posts updates that the frame drains.

(in-package #:clatter-clim)

(defstruct (irc-line (:constructor make-irc-line (kind nick text &optional (time (get-universal-time)))))
  "One rendered row in a buffer.  KIND is one of :privmsg :notice :system
:join :part :quit :topic.  NICK may be NIL for system rows."
  kind
  nick
  text
  time)

(defclass buffer ()
  ((name  :initarg :name :accessor buffer-name)
   (kind  :initarg :kind :initform :channel :accessor buffer-kind
          :documentation "One of :server :channel :query.")
   (lines :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor buffer-lines)
   (topic :initform "" :accessor buffer-topic)
   (users :initform '() :accessor buffer-users
          :documentation "List of nick strings present in a channel buffer."))
  (:documentation "A server, channel, or query conversation."))

(defun buffer-add-line (buffer line)
  "Append LINE to BUFFER.  Caller must be on the frame thread."
  (vector-push-extend line (buffer-lines buffer))
  buffer)

(defun buffer-target-p (buffer)
  "True when BUFFER is something we can send a PRIVMSG to."
  (member (buffer-kind buffer) '(:channel :query)))
