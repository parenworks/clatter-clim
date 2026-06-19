;;; build.lisp - Build clatter-clim into a standalone executable.
;;; Usage: sbcl --non-interactive --load build.lisp
;;;    or: ecl --load build.lisp

(require :asdf)

;;; Quicklisp resolves dist dependencies (bordeaux-threads, usocket, cl+ssl...).
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Make the sibling source repositories visible to ASDF: clatter-clim itself,
;;; clatter-irc, McCLIM, charmed, charmed-mcclim and clim-clog all live under
;;; ~/SourceCode, so register that tree (it wins over any dist copy, which is
;;; what we want while developing against the local checkouts).
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(merge-pathnames "SourceCode/" (user-homedir-pathname)))
   :inherit-configuration))

(format t "~&; Loading clatter-clim...~%")
#+quicklisp (funcall (find-symbol "QUICKLOAD" "QL") :clatter-clim)
#-quicklisp (asdf:load-system :clatter-clim)
(format t "~&; System loaded successfully.~%")

(defvar *output-name*
  (or (uiop:getenv "CLATTER_CLIM_OUTPUT") "clatter-clim"))

(defvar *output-path*
  (merge-pathnames *output-name*
                   (merge-pathnames "bin/" (asdf:system-source-directory :clatter-clim))))

(format t "~&; Building executable: ~A~%" *output-path*)
(ensure-directories-exist *output-path*)

#+sbcl
(sb-ext:save-lisp-and-die *output-path*
                          :toplevel #'clatter-clim:main
                          :executable t
                          :compression t
                          ;; Do not let SBCL eat --help / --version etc; the
                          ;; app passes its own arguments through.
                          :save-runtime-options nil)

#+ecl
(progn
  (asdf:make-build :clatter-clim
                   :type :program
                   :move-here *output-path*
                   :epilogue-code '(clatter-clim:main))
  (format t "~&; ECL build complete: ~A~%" *output-path*)
  (ext:quit 0))

#-(or sbcl ecl)
(error "Unsupported implementation. Use SBCL or ECL.")
