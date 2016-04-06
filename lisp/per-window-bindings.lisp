(defvar *per-window-bindings-rules* nil
  "a list of (CLASSES BINDINGS),
where each CLASS is a list of window-class names, 
where each BINDING is a (KEY . FORM). 
these are read from a file *per-window-binding-rules-fn*" )

(defvar *per-window-binding-rules-fn*
  (stumpwm-merger "per-window-bindings-rules.lisp")
  "file where *per-window-bindings-rules* should be reloadedd")


;;internal
(defvar *per-window-bindings-class-to-map* nil "hash window-class==>keymap" )

(defun per-window-bindings-reload (rules)
  (setf *per-window-bindings-class-to-map*
	(make-hash-table :test 'equal))
  (loop for (classes . bindings) in rules
     as top-copy = (deep-copy-map *top-map*)
     do (loop for (key form) in bindings
	   as defcmd-form = `(defcommand-annon ,form)
	   as cmd-name = (eval defcmd-form)
	   do (print defcmd-form)
	   do
	     (define-key top-copy (kbd key) cmd-name))
     do (loop for class in classes 
	     ;;do (format t "setting class ~A to ~A~%" class top-copy)
	   do (setf (gethash class *per-window-bindings-class-to-map*) top-copy))))

(defun per-window-bindings-reload-from-fn ()
  (load *per-window-binding-rules-fn*)
  (per-window-bindings-reload *per-window-bindings-rules*))

(defcommand per-window-bindings-reload-from-fn-cmd () ()
  "reload bindings from *per-window-binding-rules-fn*"
  (stumpwm::per-window-bindings-reload-from-fn))


(defvar *current-top-bindings* nil )
(defun focus-window-bindings (b a)
  (declare (ignore a))
  ;;(setq ab (list a b))
  (let* ((class-dest (and b (window-class b)))
	 (bindings-dest (gethash class-dest *per-window-bindings-class-to-map*))
	 (curr-bindings (car *current-top-bindings*)))
    
    (unless (eq curr-bindings bindings-dest)
      (when curr-bindings
	(pop-top-map)
	(pop *current-top-bindings*))
      (when bindings-dest
	(when debug-window-bindings (echo "pushing..."))
	(push-top-map bindings-dest)
	(push bindings-dest *current-top-bindings*)))))

(defmacro defcommand-annon  (&rest forms)
  (let* ((name (gentemp "autogen-cmd" "STUMPWM"))
	 (docstring (format nil "~A. contents: ~A" name forms))
	 (form `(progn
		  (defcommand ,name () () ,docstring ,@forms)
		  ,(symbol-name name))))
    (print form)
    form))




(per-window-bindings-reload-from-fn)
(add-hook *focus-window-hook* 'focus-window-bindings)
