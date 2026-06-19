;;;; package.lisp - package definition for clatter-clim

(in-package #:cl-user)

(defpackage #:clatter-clim
  (:use #:clim-lisp #:clim)
  (:nicknames #:cclim)
  (:local-nicknames (#:irc #:clatter-irc)
                    (#:bt  #:bordeaux-threads))
  (:documentation "A McCLIM IRC client front-end built on clatter-irc.")
  (:export #:clatter-clim
           #:main
           #:run
           #:run-native
           #:run-terminal
           #:run-web))
