(in-package :STUMPWM)

(defun init ()
  (defparameter *font-size* 40)
  (defvar *background-image-fn* #P"~/.background-image-symlink")

  (defvar *init-errors* nil)

  (setf *max-last-message-size* SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)

  (defmacro safe-init-sexp (&body body)
    "evaluate an expression, handling and recording errors"
    `(handler-case
         (progn ,@body)
       (error (err)
         (push (list ',body err) *init-errors*)
         (message-wrapped "error while loading: ~A~%: '~A'" ',body err))))

  (with-message-queuing t
    (swank-start)
    (decorations-init)
    (xinitrc-init)
    ;; TODO remove side-effects. add "init" method
    (load-stumpwmrc-file "top-map-bindings.lisp")
    (init-top-map-bindings)
    ;; url-launcher may fail if not connected to the internet, .authinfo doesn't exist, etc
    (url-launcher-init)
    (text-shortcuts-init)
    (brightness-init)
    (x-service:start 1959)
    ;; TODO remove these
    (defvar *battery-notification-thread*)
    (defvar *sms-fanout-reconnect-thread*)
    (def-thread-start *battery-notification-thread*
        (battery-info-check-notify-loop))
    (def-thread-start *sms-fanout-reconnect-thread*
        (sms-fanout-client:reconnect-loop))
    (setf *startup-message* nil)
    (focus-group-hook-update-env (current-group)) ;; should run before the terminal emulator
    (startup-apps-run)
    (decorations-init)
    (safe-init-sexp (contacts:contacts-load)) ;; contacts file may not exist
    (setf lparallel:*kernel* (lparallel:make-kernel 3))
    (message "done loading .stumpwmrc")))
