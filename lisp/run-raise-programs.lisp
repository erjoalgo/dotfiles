(defun define-run-or-pull-program (name
				   &key
				     (raise-key (format nil "H-~A" (char name 0)))
				     (pull-key (string-upcase raise-key))
				     (cmd name)
				     (classes (list (string-capitalize name)))
				     (all-screens nil))
  
  (loop for (pull-or-raise-fun key) in `((raise-window ,raise-key)
					 (pull-window ,pull-key))
       
     as cmd-name = (gentemp (format nil "auto-gen-~A-~A"
				    (symbol-name pull-or-raise-fun)
					;(symbol-name pull-or-raise-fun)
				    ;; (if (subtypep (type-of pull-or-raise-fun) 'STANDARD-GENERIC-FUNCTION)
				    ;; 	(slot-value  pull-or-raise-fun 'SB-PCL::NAME)
				    ;; 	(symbol-name pull-or-raise-fun))
				    ;;(subtypep (type-of #'pull-window ) 'STANDARD-GENERIC-FUNCTION)
				    name))
     as cmd-name-string = (symbol-name cmd-name)
     ;;as fun = (eval `(function ,pull-or-raise-fun));;TODO !
     as fun = pull-or-raise-fun
     as form = `(defcommand ,cmd-name nil nil ,(format nil "doc: ~A" cmd-name-string)
			    (let* ((win-list ,(if all-screens `(screen-windows (current-screen))
						  `(group-windows (current-group))))
				   (curr-win (current-window))
				   (classes (list ,@classes))
				   (win (loop for win in win-list
					   thereis (and win
							(not (eq win curr-win))
							(member (window-class win) classes :test 'equal)
							win))))
			      (if win
				  (progn (,fun win)
					 (focus-all win))
				  (unless (and curr-win
					       (member (window-class curr-win) classes))
				    (run-shell-command ,cmd)))))
     unless (null key)
     do (progn
	  (print form)
	  (eval form)
	  (define-key *top-map* (kbd raise-key) cmd-name-string))))

(define-run-or-pull-program "firefox"
    :raise-key "H-f"
    :cmd "firefox --no-remote -P default"
    :classes *browser-classes* :all-screens t)

(define-run-or-pull-program "x-terminal-emulator"
    :raise-key "H-c"
    :cmd "roxterm" 
    :classes (list "X-terminal-emulator" "Roxterm" "roxterm"))

