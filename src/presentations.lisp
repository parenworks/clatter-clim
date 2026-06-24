;;;; presentations.lisp - CLIM presentation types for IRC nouns
;;;;
;;;; Making nicks, channels and buffers presentations is the whole point of
;;;; doing this in CLIM: clicking them invokes translators (see the
;;;; translators in commands.lisp) on every backend for free.

(in-package #:clatter-clim)

(define-presentation-type nick ()
  :inherit-from 'string
  :description "an IRC nickname")

(define-presentation-type irc-channel ()
  :inherit-from 'string
  :description "an IRC channel name")

(define-presentation-type url ()
  :inherit-from 'string
  :description "a web URL")

(define-presentation-type buffer ()
  :description "a conversation buffer")

;;; A custom-drawn clickable button.  The presentation object is a UI-BUTTON
;;; struct (see frame.lisp); clicking runs its action via a translator.
(define-presentation-type ui-button ()
  :description "a button")

;;; A pending incoming DCC offer.  The object is a DCC-OFFER struct (model.lisp);
;;; click accepts it, the right-click menu offers reject (see commands.lisp).
(define-presentation-type dcc-offer ()
  :description "a DCC offer")

(define-presentation-method present (object (type buffer) stream view &key)
  (declare (ignore view))
  (format stream "~A" (buffer-name object)))
