(in-package :STUMPWM)

(defvar *battery-notification-thread*)

(defvar *sms-fanout-reconnect-thread*)

(defvar *init-errors*)

(defmacro safe-sexp (&body body)
  "Evaluate an expression, handling and recording errors"
  `(handler-case
       (progn ,@body)
     (error (err)
       (push (list ',body err) *init-errors*)
       (message-wrapped "error while loading: ~A~%: '~A'" ',body err))))

(defun init ()
  (setf *max-last-message-size* SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY)
  (with-message-queuing t
    (swank-start)
    (setf lparallel:*kernel* (lparallel:make-kernel 10))
    (lparallel-future (with-message-queuing t (xinitrc-init)))
    ;; TODO remove side-effects. add "init" method
    (load-stumpwmrc-file "top-map-bindings.lisp")
    ;; url-launcher may fail if not connected to the internet, .authinfo doesn't exist, etc
    (safe-sexp (url-launcher-init))
    (text-shortcuts-init)
    (brightness-init)
    (x-service:start 1959)
    ;; TODO remove these
    (def-thread-start *battery-notification-thread*
      (battery-info-check-notify-loop))
    (def-thread-start *sms-fanout-reconnect-thread*
      (sms-fanout-client:reconnect-loop))
    (setf *startup-message* nil)
    (focus-group-hook-update-env (current-group)) ;; should run before the terminal emulator
    (startup-apps-run)
    ;; (safe-sexp (contacts:contacts-load)) ;; contacts file may not exist
    (message "done loading .stumpwmrc. ~D errors:~%~{~A~^~%~}"
             (length *init-errors*)
             *init-errors*)))
