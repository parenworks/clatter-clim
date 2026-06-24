;;;; model.lisp - the IRC view model
;;;;
;;;; Plain data, no CLIM and no clatter-irc here.  The model is mutated
;;;; only on the CLIM frame thread (see bridge.lisp): the IRC reader thread
;;;; never touches it directly, it posts updates that the frame drains.

(in-package #:clatter-clim)

(defclass network ()
  ((connection :initarg :connection :initform nil :accessor network-connection
               :documentation "The clatter-irc connection for this server.")
   (label      :initarg :label :accessor network-label
               :documentation "Display name and identity for this network, the
server hostname as the user typed it.")
   (dcc-manager :initform nil :accessor network-dcc-manager
                :documentation "Lazily-created clatter-irc DCC manager for this
connection (see NETWORK-DCC in commands.lisp)."))
  (:documentation "One IRC server connection.  A frame may hold several; every
buffer belongs to exactly one network, so the same channel name on two networks
stays distinct."))

(defstruct (irc-line (:constructor make-irc-line (kind nick text &optional (time (get-universal-time)) data)))
  "One rendered row in a buffer.  KIND is one of :privmsg :notice :system
:join :part :quit :topic :dcc-offer.  NICK may be NIL for system rows.  DATA
carries kind-specific payload, e.g. the DCC-OFFER object for a :dcc-offer row."
  kind
  nick
  text
  time
  data)

(defstruct (dcc-offer (:constructor make-dcc-offer (network connection)))
  "A pending incoming DCC offer, wrapped so an offer line can be a clickable
presentation that knows both its NETWORK and the underlying clatter-irc DCC
CONNECTION (a dcc-chat or dcc-send)."
  network
  connection)

(defclass buffer ()
  ((name  :initarg :name :accessor buffer-name)
   (kind  :initarg :kind :initform :channel :accessor buffer-kind
          :documentation "One of :server :channel :query.")
   (lines :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor buffer-lines)
   (topic :initform "" :accessor buffer-topic)
   (unread :initform 0 :accessor buffer-unread
           :documentation "Count of unseen message lines since this buffer was last current.")
   (ping :initform nil :accessor buffer-ping
         :documentation "True when an unseen line in this buffer mentioned our nick.")
   (users :initform '() :accessor buffer-users
          :documentation "List of nick strings present in a channel buffer.")
   (network :initarg :network :initform nil :accessor buffer-network
            :documentation "The NETWORK this buffer belongs to.")
   (dcc :initarg :dcc :initform nil :accessor buffer-dcc
        :documentation "For a :dcc buffer, the clatter-irc dcc-chat connection
that SAY routes its text to."))
  (:documentation "A server, channel, or query conversation."))

(defun buffer-add-line (buffer line)
  "Append LINE to BUFFER.  Caller must be on the frame thread."
  (vector-push-extend line (buffer-lines buffer))
  buffer)

(defun buffer-target-p (buffer)
  "True when BUFFER is something we can send a PRIVMSG to."
  (member (buffer-kind buffer) '(:channel :query)))
