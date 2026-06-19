;;;; clatter-clim.asd - ASDF system definition for clatter-clim
;;;;
;;;; A McCLIM IRC client built on clatter-irc.  The application is written
;;;; once against CLIM presentations and commands and runs unchanged on
;;;; three backends:
;;;;
;;;;   - the stock McCLIM CLX backend          (native X11/Wayland)
;;;;   - mcclim-charmed                          (terminal, over SSH)
;;;;   - clim-clog                               (HTML5 canvas in a browser)
;;;;
;;;; Only the core system is declared here.  The terminal and web backends
;;;; are loaded on demand by the RUN-TERMINAL and RUN-WEB entry points so
;;;; the core stays backend-agnostic.

(defsystem "clatter-clim"
  :name "clatter-clim"
  :version "0.0.1"
  :author "Glenn Thompson"
  :license "MIT"
  :homepage "https://github.com/parenworks/clatter-clim"
  :description "A McCLIM IRC client built on clatter-irc, portable across the CLX, terminal (mcclim-charmed) and web (clim-clog) backends."
  :depends-on ("mcclim" "clatter-irc" "bordeaux-threads")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "model")
                             (:file "presentations")
                             (:file "frame")
                             (:file "commands")
                             (:file "bridge")
                             (:file "main")))))
