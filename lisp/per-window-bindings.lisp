(in-package :STUMPWM)

;; (defpackage #:stumpwm-per-window-bindings
;;   (:export
;;    #:per-window-bindings-reload
;;    #:per-window-bindings-reload-from-fn))

(defvar *per-window-bindings-rules* nil
  "a list of (CLASSES BINDINGS),
where each CLASS is a list of window-class names,
where each BINDING is a (KEY . FORM).
these are read from a file *per-window-binding-rules-fn*" )

(defvar *per-window-binding-rules-fn*
  (merge-pathnames "per-window-bindings-rules.lisp" *stumpwm-top-directory*)
  "file where *per-window-bindings-rules* should be reloaded.
it contains lisp code which sets the *per-window-bindings-rules* value")


;;internal
(defvar *per-window-bindings-class-to-map*
  (make-hash-table :test 'equal);;dummy value in case it isn't initialized properly
  "hash map: window-class to keymap" )

(defun per-window-bindings-reload (rules)
  (setf *per-window-bindings-class-to-map*
	(make-hash-table :test 'equal))
  (loop for (classes . bindings) in rules
     as top-copy = (stumpwm::deep-copy-map STUMPWM::*top-map*)
     do (loop for (key form) in bindings
	   as defcmd-form = `(defcommand-annon ,form)
	   as cmd-name = (eval defcmd-form)
	   ;;do (print defcmd-form)
	   do
	     (stumpwm:define-key top-copy (stumpwm:kbd key) cmd-name))
     do (loop for class in classes
	   ;;do (format t "setting class ~A to ~A~%" class top-copy)
	   do (setf (gethash (string-downcase class)
			     *per-window-bindings-class-to-map*)
                    top-copy))))

(defun per-window-bindings-reload-from-fn ()
  (load *per-window-binding-rules-fn*)
  (per-window-bindings-reload *per-window-bindings-rules*))

(defcommand per-window-bindings-reload-from-fn-cmd () ()
  "reload bindings from *per-window-binding-rules-fn*"
  (stumpwm::per-window-bindings-reload-from-fn))

(defvar *current-top-bindings* nil)

(defvar dbg nil)

(defun update-window-bindings (curr-win)
  (let (msgs)
    (push (format nil "update-window-bindings invoked on curr: ~A (~A)"
                  curr-win (when curr-win (window-class curr-win)))
          msgs)
    (let* ((curr-bindings (car *current-top-bindings*))
           class-dest bindings-dest)

      (when curr-win
        (setf class-dest (window-class curr-win)
              bindings-dest (gethash (string-downcase class-dest)
                                     *per-window-bindings-class-to-map*))
        (when bindings-dest
          (push (format nil "found bindings") msgs)))

      (unless (eq curr-bindings bindings-dest)
          (push (format nil "changing bindings from ~A to ~A..."
                        (when (current-window) (window-class (current-window)))
                        class-dest)
                msgs)
          (when (or (and (null class-dest)
                         (current-window)
                         (or
                          (member (string-downcase
                                   (window-class (current-window)))
                                  *browser-classes*
                                  :test #'equal :key #'string-downcase)))
                    (member "changing bindings from Chromium to NIL..." msgs
                            :test 'equal))
            (message "changing from browser to nil?")
            (return-from update-window-bindings))
        (when curr-bindings
            (push (format nil "popping old bindings") msgs)
          (pop-top-map)
          (pop *current-top-bindings*))
        (when bindings-dest
          (push (format nil "pushing ~A" class-dest) msgs)
          (push-top-map bindings-dest)
          (push bindings-dest *current-top-bindings*))))
    (push (cons (GET-UNIVERSAL-TIME) msgs) dbg)))

(defmacro defcommand-annon  (name &rest forms)
  (let* ((name (intern name "STUMPWM"))
	 (docstring (format nil "~A. auto-defined: ~A" name forms)))
    `(progn
       (message ,(format nil "defining ~A" name))
       (STUMPWM::defcommand ,name () () ,docstring ,@forms))))

(defun focus-window-bindings-hook (new old)
  (declare (ignore old))
  (assert new)
  (update-window-bindings new))

(add-hook STUMPWM::*focus-window-hook* 'focus-window-bindings-hook)

(defun destroy-window-hook (destroyed)
  (declare (ignore destroyed))
  (update-window-bindings nil))

(add-hook STUMPWM:*destroy-window-hook* 'destroy-window-hook)

;; STUMPWM:*focus-frame-hook* runs before
;; (screen-current-window (current-screen)) is defined...
;; so this won't work:
;; (add-hook STUMPWM:*focus-frame-hook* 'focus-window-bindings)

(defun focus-frame-hook (new old)
  (declare (ignore old))
  (update-window-bindings (frame-window new)))

(add-hook STUMPWM:*focus-frame-hook* 'focus-frame-hook)
