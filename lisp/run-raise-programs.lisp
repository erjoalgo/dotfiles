(defun raise-pull-or-run-win (win-classes command &optional pull-p all-screens)
  (let* ((win-list (if all-screens (screen-windows (current-screen))
		       (group-windows (current-group))))
	 (curr-win (current-window))
	 (win-matches (lambda (win)
			(member (window-class win) win-classes :test 'equal)))
	 (cands (remove-if-not
		 (lambda (win)
		   (and (not (equal win curr-win))
			(funcall win-matches win)))
		 win-list))
	 (cand (car cands)))
    (if cands
	(progn (funcall (if pull-p 'pull-window
			    'raise-window)
			cand)
	       (focus-all cand))
	(unless (and curr-win
		     (funcall win-matches curr-win))
	  (run-shell-command command)))))

(defmacro define-run-or-pull-program (name
				   &key
				     (raise-key (format nil "H-~A" (char name 0)))
				     (pull-key (string-upcase raise-key))
				     (cmd name)
					      (classes `(list ,(string-capitalize name)))
				     (all-screens nil))

  `(progn
     ,@(loop for (pull-or-raise-fun key) in `((raise-window ,raise-key)
					      (pull-window ,pull-key))

     as cmd-name = (gentemp (format nil "auto-gen-~A-~A"
				    (symbol-name pull-or-raise-fun)
					;(symbol-name pull-or-raise-fun)
				    ;; (if (subtypep (type-of pull-or-raise-fun) 'STANDARD-GENERIC-FUNCTION)
				    ;; 	(slot-value  pull-or-raise-fun 'SB-PCL::NAME)
				    ;; 	(symbol-name pull-or-raise-fun))
				    ;;(subtypep (type-of #'pull-window ) 'STANDARD-GENERIC-FUNCTION)
				    name))
	  as pull-p = (eq pull-or-raise-fun #'pull-window)
     as cmd-name-string = (symbol-name cmd-name)
     ;;as fun = (eval `(function ,pull-or-raise-fun));;TODO !
     as fun = pull-or-raise-fun
	  as doc = (format nil "doc: ~A" cmd-name-string)
	  do (print "classes")
	  do (print classes)
	  append
	    `(
	      (defcommand ,cmd-name nil nil ,doc
			  (raise-pull-or-run-win ,classes ,cmd ,pull-p ,all-screens))
	      ,(unless (null key)
		       `(define-key *top-map* (kbd ,key) ,cmd-name-string))))))

(define-run-or-pull-program "firefox"
    :raise-key "H-f"
    :pull-key "H-F"
    :cmd "firefox --no-remote -P default"
    :classes *browser-classes* :all-screens t)

(define-run-or-pull-program "x-terminal-emulator"
    :raise-key "H-c"
    :cmd "roxterm"
    :classes (list "X-terminal-emulator" "Roxterm" "roxterm"))

(define-run-or-pull-program "emacs"
    :pull-key "H-E")

