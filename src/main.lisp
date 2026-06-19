;;;; main.lisp - entry points, one per backend
;;;;
;;;; The frame, panes, commands, presentations and bridge are identical for
;;;; all three.  Only the port differs.  The terminal and web backends are
;;;; loaded lazily so the core system stays free of those dependencies.

(in-package #:clatter-clim)

(defun %limit-font-path ()
  "Stop McCLIM from scanning every system TrueType font at startup.
Per jackdaniel: set mcclim-truetype:*truetype-font-path* to the empty string
before any port is initialised.  Done by symbol lookup so the core system
does not have to depend on the truetype backend being present."
  (let ((sym (find-symbol "*TRUETYPE-FONT-PATH*" "MCCLIM-TRUETYPE")))
    (when (and sym (boundp sym))
      (setf (symbol-value sym) "")
      t)))

(defun %apply-dark-gadget-theme ()
  "Recolour McCLIM's built-in 3D gadget chrome to match the dark palette.

McCLIM draws scrollbars, button bevels and pane borders from four global
greys (its \"Motif-ish\" defaults) in CLIM-INTERNALS.  They are DEFPARAMETERs and
CLOS default-initargs re-read them per gadget, so overriding them before the
frame is built restyles the otherwise grey 1985-era widgets.  Done by symbol
lookup so the core does not hard-depend on these internals existing."
  (flet ((set-grey (name value)
           (let ((sym (find-symbol name "CLIM-INTERNALS")))
             (when (and sym (boundp sym))
               (setf (symbol-value sym) value)))))
    (set-grey "*3D-NORMAL-COLOR*" (make-gray-color 0.22))   ; button face / thumb
    (set-grey "*3D-LIGHT-COLOR*"  (make-gray-color 0.34))   ; top-left bevel
    (set-grey "*3D-DARK-COLOR*"   (make-gray-color 0.08))   ; bottom-right bevel
    (set-grey "*3D-INNER-COLOR*"  (make-gray-color 0.13))   ; scrollbar trough
    ;; Slimmer scrollbars than the 16px Motif default; our custom HANDLE-REPAINT
    ;; (frame.lisp) draws a thin rounded thumb within this width.
    (let ((sym (find-symbol "*SCROLLBAR-THICKNESS*" "CLIM-INTERNALS")))
      (when (and sym (boundp sym))
        (setf (symbol-value sym) 12)))))

(defun %boot (&key (new-process t) make-frame)
  "Run the frame returned by MAKE-FRAME, optionally in its own CLIM process.
The font path must be limited before MAKE-FRAME creates the port."
  (%limit-font-path)
  (%apply-dark-gadget-theme)
  (load-config)
  (flet ((launch () (run-frame-top-level (funcall make-frame))))
    (if new-process
        (clim-sys:make-process #'launch :name "clatter-clim")
        (launch))))

(defun run-native (&key (new-process t))
  "Run on the default McCLIM backend (CLX, native X11/Wayland)."
  (%boot :new-process new-process
         :make-frame (lambda () (make-application-frame 'clatter-clim))))

(defun run (&rest args)
  "Default entry point: the native backend."
  (apply #'run-native args))

(defun main ()
  "Toplevel for the built binary: run on the native backend in the
foreground so the image stays alive until the frame exits."
  (run-native :new-process nil)
  (uiop:quit 0))

(defun run-terminal ()
  "Run in the terminal via the mcclim-charmed backend.
Loads :mcclim-charmed on demand."
  (asdf:load-system :mcclim-charmed)
  (%limit-font-path)
  (load-config)
  ;; clim-charmed:run-frame-on-charmed-with-interactor handles port lifecycle
  ;; and terminal restoration; it expects an application-frame class name.
  (uiop:symbol-call :clim-charmed :run-frame-on-charmed-with-interactor
                    'clatter-clim))

(defun run-web (&key (port 8080) (new-process t))
  "Run in a browser tab via the clim-clog backend.
Loads :clim-clog on demand.

NOTE: clim-clog currently drives its own demo frame from a CLOG on-new-window
boot function (clim-clog::start-frame).  Running an arbitrary frame class on
it needs a small generic entry point on the clim-clog side; until that lands,
this delegates to whatever clim-clog exposes and is the one Phase-3 seam.
See ROADMAP.org."
  (asdf:load-system :clim-clog)
  (%boot :new-process new-process
         :make-frame
         (lambda ()
           (let ((port (find-port :server-path (list :clog :port port))))
             (make-application-frame 'clatter-clim
                                     :frame-manager (first (climi::frame-managers port)))))))
