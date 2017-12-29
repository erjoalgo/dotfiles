(defun raise-pull-or-run-win (win-classes command &optional pull-p all-screens)
  (let* ((win-list (if all-screens (screen-windows (current-screen))
		       (group-windows (current-group))))
	 (curr-win (current-window))
	 (win-matches (lambda (win)
			(member (string-downcase (window-class win))
				win-classes :test 'equal)))
	 (cands (remove-if-not
		 win-matches
		 win-list))
	 (cand-no-curr (car (remove curr-win cands))))
    (if cand-no-curr
	(progn (funcall (if pull-p 'pull-window
			    'raise-window)
			cand-no-curr)
	       (focus-all cand-no-curr))
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
					 name))
	  as pull-p = (eq pull-or-raise-fun 'pull-window)
	  as cmd-name-string = (symbol-name cmd-name)
	  as fun = pull-or-raise-fun
	  as doc = (format nil "doc: ~A" cmd-name-string)
	  append
	    `(
	      (defcommand ,cmd-name nil nil ,doc
			  (raise-pull-or-run-win (mapcar 'string-downcase ,classes)
						 ,cmd ,pull-p ,all-screens))
	      ,(unless (null key)
		       `(define-key *top-map* (kbd ,key) ,cmd-name-string))))))

(define-run-or-pull-program *browser-name*
    :raise-key "H-f"
    :pull-key "H-F"
    :classes *browser-classes* :all-screens t)

(define-run-or-pull-program "x-terminal-emulator"
    :raise-key "H-c"
    :cmd (if (equal "" (run-shell-command "which roxterm" t))
	     ;; "xterm"
	     ;; "xterm -fa 'Monospace' -fs 20 -ls -xrm 'XTerm*selectToClipboard: true'"
	     "gnome-terminal --hide-menubar"
	     "roxterm")
    :classes (list "X-terminal-emulator" "Roxterm" "roxterm"
		   "xterm" "XTerm" "Gnome-terminal"))

(define-run-or-pull-program "emacs"
    :pull-key "H-E")

(define-run-or-pull-program "android-studio"
    :classes '("jetbrains-studio")
    :cmd "~/Downloads/android-studio/bin/studio.sh"
    :raise-key "H-r")

(define-run-or-pull-program "zathura")

;; Warning: these bindings affect the *top-map*, which is
;; later deep-copied onto other per-window bindings.
;; Changes won't take effect on existing deep-copies of
;; *top-map*
;; for changes to take effect, 'per-window-bindings' and
;; top-map-bindings.lisp should be evaled in that order
