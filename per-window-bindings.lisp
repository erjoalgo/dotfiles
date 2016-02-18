
;;(setq debug-window-bindings t)
(setq WINDOW-BINDING-RULES-FN "per-window-bindings-rules.lisp")
(setq debug-window-bindings t)
(setq debug-window-bindings nil )
(defun update_bindings_hash ()
  (let* (
	 (bindings-spec
	  (eval (read-from-string (file-string (stumpwm-merger WINDOW-BINDING-RULES-FN)))))
	 (hash (make-hash-table :test 'equal))
	 )
    (print (format nil "number of bindings-spec ~D" (length bindings-spec)))
    
    (mapcar (lambda (args)
		       
	      (let* (
		     ;;copy the map
		     (top-copy (deep-copy-map *top-map*))
		     (matching-classes (car args))
		     (bindings (cdr args))
		     )

		(print (format nil "~D matching classes, ~D bindings"
				      (length matching-classes)
				      (length bindings)))
		
		;;add all the actions to the copied map
		(mapcar (lambda (key-action)
			  (let* ((key (car key-action))
				 (actions (cdr key-action)))
			    
			  (define-key top-copy
			      (kbd key) (defcommand-annon actions))
			  ))
			bindings)
		;;associate all the matching window classes to the copied map
		(mapcar (lambda (window-class-string)
			  (setf (gethash window-class-string hash)
				top-copy))
			matching-classes)
		))
	    bindings-spec)
    (setq *top-hash-map* hash)
    )
  )
   
(defun focus-window-bindings (b a)
  
  )

'(defun focus-window-bindings (b a)
  (setq ab (list a b))
  ;;make this a little bit faster?
  (let* (
	 (class-source (and a (window-class a)))
	 (class-dest (and b (window-class b)))
	 (bindings-source (gethash class-source *top-hash-map*))
	 (bindings-dest (gethash class-dest *top-hash-map*))
	 )
    (when (not (eq bindings-source bindings-dest))
	(when bindings-source
	  ;;pop previous top map from source
	  (when debug-window-bindings (echo "popping..."))
	  (pop-top-map))
	(when bindings-dest
	  (when debug-window-bindings (echo "pushing..."))
	  (push-top-map bindings-dest))
	)
    )
  )

(setq *current-top-bindings* nil )
(defun focus-window-bindings (b a)
  (setq ab (list a b))
  (let* (
	 (class-dest (and b (window-class b)))
	 (bindings-dest (gethash class-dest *top-hash-map*))
	 (curr-bindings (car *current-top-bindings*))
	 )
    
    (when (not (eq curr-bindings bindings-dest))
      (when curr-bindings
	(when debug-window-bindings (echo "popping..."))
	(pop-top-map)
	(setq *current-top-bindings* (cdr *current-top-bindings*))
	)
      (when bindings-dest
	  (when debug-window-bindings (echo "pushing..."))
	  (push-top-map bindings-dest)
	  (setq *current-top-bindings* (cons bindings-dest *current-top-bindings*))
	  )
	  ;;pop previous top map from source
	)
    )
  )

(defun destroy-window-bindings (win)
  
  )
(add-hook *destroy-window-hook* 'destroy-window-bindings)
(defun focus-group-bindings (b a)
  ;;(pop-top-map)
  )



(setq annon-funs-hash (make-hash-table :test 'equal))
(setq autogen-command-index 0)

(defun defcommand-annon  (command)
  (let* (
	 (name (gethash command annon-funs-hash))
	 ;(namesmym nil )
	 ;;(form nil )
	 )
	
    ;(if (not name)
    (if t
	(progn 
	  (setq name (format nil "AUTOGENCMD-~D"  autogen-command-index))
	  (setq autogen-command-index (1+ autogen-command-index ))
	  (setf (gethash command annon-funs-hash) name)
	  (let* (
		 (actions 
		  (if debug-window-bindings
		      (cons
		       `(echo ,(format nil "running anon-cmd ~A" name))
		       command)
		      command
		      ))
		 
		 (defcmd-form `(defcommand ,(intern name) () ()
				 ,(prin1-to-string (cons name actions))
				 ,@actions))
		 )
	    ;(print defcmd-form)
	    ;(print actions)
	    (eval defcmd-form)
	    ))
      )
    name
    )
  )

(defcommand update_bindings_hash_cmd () ()
  (stumpwm::update_bindings_hash)
  )

(update_bindings_hash)
(add-hook *focus-window-hook* 'focus-window-bindings)
(add-hook *focus-group-hook* 'focus-group-bindings)

;(setq *focus-window-hook* nil )
