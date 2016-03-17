(defun define-run-or-pull-program (name raise-key
					&key
					(pull-key (string-upcase raise-key))
					(cmd name)
					(classes (funcall (compose 'list 'string-capitalize) name))
					(all-screens nil )
					
					)
  (let* (
	 (classes-hash (hash-from-list classes))
	 
	 (form-installer
	  (lambda (pull-or-raise-opt cmd-name-suffix key)
	    (let* (
		   (cmd-name-sub (format nil "autogen-~A-~A" cmd-name-suffix name))
		   (form `(defcommand ,(intern cmd-name-sub) () ()
			    (let* (
				   (curr-win (current-window))
				   (matching
				    (remove-if-not
				     (lambda (win)
				       (and win (not (eq win curr-win))
					    (gethash (window-class win) ,classes-hash)
					    ))
				     ,(if all-screens
					  `(screen-windows (current-screen))
					`(group-windows (current-group)))
				     )
				    )
				   )
			      (if matching
				  (progn 
				    (,pull-or-raise-opt (car matching))
				    (focus-all (car matching)))
				(when 
				    (not (and
					  (current-window)
					  (gethash (window-class (current-window)) ,classes-hash)))
				  (run-shell-command ,cmd)
				  )
				)
			      )
			    ))
		   (kmap-installer (lambda (kmap)
				     (print `(define-key kmap (kbd ,key) ,cmd-name-sub))
				     (define-key kmap (kbd key) cmd-name-sub)
				     ))
		   )
	      (progn
		    (eval form)
		    (funcall kmap-installer *top-map*)
		    (maphash (lambda (k v) (funcall kmap-installer v)) *top-hash-map*)
		    )
	      (print form)
	      ;;(print `(define-key *top-map* (kbd ,key) ,cmd-name-sub))
	      ;;(define-key kmap (kbd key) cmd-name-sub)
	      ))
	  )
	 )
    (funcall form-installer 'pull-window "pull" pull-key)
    (funcall form-installer 'raise-window "raise" raise-key)
    )
  )

(setq run_raise_pull_list
      `(
	;;("iceweasel" "H-f" :cmd "firefox --no-remote -P default"
	("firefox" "H-f"
		   :cmd "firefox --no-remote -P default"
		   :classes ,*browser-classes* :all-screens t)
	("x-terminal-emulator" "H-c" :cmd "roxterm" :classes (list "X-terminal-emulator" "Roxterm" "roxterm"))
	;;("emacs" "H-e" :classes (list "emacs" "Emacs"))
	;; ("skype" "H-s")
	;; ("jin" "H-j" :classes '("free-jin-JinApplication"))
	)
      )
(mapcar (curry 'apply 'define-run-or-pull-program) run_raise_pull_list)

