(defpackage #:stumpwm-per-window-bindings
  (:export
   #:per-window-bindings-reload
   #:per-window-bindings-reload-from-fn))

(defvar *per-window-bindings-rules* nil
  "a list of (CLASSES BINDINGS),
where each CLASS is a list of window-class names,
where each BINDING is a (KEY . FORM).
these are read from a file *per-window-binding-rules-fn*" )

(defvar *per-window-binding-rules-fn*
  (STUMPWM::stumpwm-merger "per-window-bindings-rules.lisp")
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
     as top-copy = (deep-copy-map STUMPWM::*top-map*)
     do (loop for (key form) in bindings
	   as defcmd-form = `(defcommand-annon ,form)
	   as cmd-name = (eval defcmd-form)
	   ;;do (print defcmd-form)
	   do
	     (define-key top-copy (kbd key) cmd-name))
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

(defun update-window-bindings (curr-win)
  (let* ((b curr-win)
         (curr-bindings (car *current-top-bindings*))
         class-dest bindings-dest)

    (when b
      (setf class-dest (window-class b)
            bindings-dest (gethash (string-downcase class-dest)
				   *per-window-bindings-class-to-map*)))

    (unless (eq curr-bindings bindings-dest)
      (when curr-bindings
	(pop-top-map)
	(pop *current-top-bindings*))
      (when bindings-dest
	(push-top-map bindings-dest)
	(push bindings-dest *current-top-bindings*)))))

(defmacro defcommand-annon  (&rest forms)
  (let* ((name (gentemp "autogen-cmd" "STUMPWM"))
	 (docstring (format nil "~A. contents: ~A" name forms))
	 (form `(progn
		  (STUMPWM::defcommand ,name () () ,docstring ,@forms)
		  ,(symbol-name name))))
    form))

(per-window-bindings-reload-from-fn)

(defun focus-window-bindings-hook (new old)
  (declare (ignore old))
  (assert new)
  (update-window-bindings new))

(add-hook STUMPWM::*focus-window-hook* 'focus-window-bindings-hook)

(defun destroy-window-hook (destroyed)
  (declare (ignore destroyed))
  (update-window-bindings nil))

(add-hook STUMPWM::*UNMAP-WINDOW-HOOK* 'unmap-window-hook)
(add-hook STUMPWM:*destroy-window-hook* 'destroy-window-hook)
