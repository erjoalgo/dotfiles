(defvar pid-original-group-alist nil
  "An alist (PID . (GROUP . TIMESTAMP)) that records
the group/workspace in which a process was originally started")

(defvar raise-window-in-original-group-secs nil
  "If non-nil, any window from a process originally
started in group A that is raised in group B
is moved back to group A if its process
was started less than RAISE-WINDOW-IN-ORIGINAL-GROUP-SECS
seconds ago")
(setf raise-window-in-original-group-secs 10)

(defun raise-window-in-original-group (new-win)
  (let ((new-win-pid (window-pid new-win)))
    (if (not new-win-pid)
        (warn "unable to locate pid for window: ~A" new-win)
        (setf pid-original-group-alist
              (loop with now = (GET-UNIVERSAL-TIME)
                 for entry in pid-original-group-alist
                 as keep = (destructuring-bind (pid . (group . timestamp))
                               entry
                             (when (and raise-window-in-original-group-secs
                                        (< (- now timestamp) raise-window-in-original-group-secs))
                               (if (= new-win-pid pid)
                                   (progn (move-window-to-group new-win group)
                                          nil);; remove it from the list
                                   (progn t))))
                 if keep collect entry)))))

(add-hook stumpwm:*new-window-hook* 'raise-window-in-original-group)

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
	  (let ((proc
                 ;; this creates an extra shell whose pid doesn't match window pid
                 ;; (run-shell-command command)
                 (run-prog command :args nil
                           :wait nil
                           :search t)))
            (when raise-window-in-original-group-secs
              (push (cons (sb-ext:process-pid proc)
                          (cons (current-group) (GET-UNIVERSAL-TIME)))
                    pid-original-group-alist)))))))

(defmacro define-run-or-pull-program (name
				      &key
					(raise-key (format nil "H-~A" (char name 0)))
					(pull-key (string-upcase raise-key))
					(cmd name)
					(classes `(list ,(string-capitalize name)))
                                      (all-screens nil)
                                      (keymap '*top-map*))

  `(progn
     ,@(loop for (pull-or-raise-fun key) in `((raise-window ,raise-key)
					      (pull-window ,pull-key))

	  as cmd-name = (intern (format nil "~A-~A" (symbol-name pull-or-raise-fun) name))
	  as pull-p = (eq pull-or-raise-fun 'pull-window)
	  as cmd-name-string = (symbol-name cmd-name)
	  as fun = pull-or-raise-fun
	  as doc = (format nil "autogen ~A '~A'" (if pull-p "pull" "raise") cmd)
	  append
	    `(
	      (defcommand ,cmd-name nil nil ,doc
			  (raise-pull-or-run-win (mapcar 'string-downcase ,classes)
						 ,cmd ,pull-p ,all-screens))
	      ,(unless (null key)
		 `(define-key ,keymap (kbd ,key) ,cmd-name-string))))))

(define-run-or-pull-program "BROWSER"
    :cmd browser-name
    :raise-key "H-f"
    :pull-key "H-F"
    :classes browser-classes)

(define-run-or-pull-program "X-TERMINAL-EMULATOR"
    :raise-key "H-c"
    :cmd (trim-spaces
          (run-shell-command
           "which konsole roxterm gnome-terminal xterm | head -1" t))
    :classes (list "Konsole" "X-terminal-emulator" "Roxterm" "roxterm"
		   "xterm" "XTerm" "Gnome-terminal"))

(defparameter emacs-classes
  (list "emacs" "GoogleEmacs"))

(define-run-or-pull-program "emacs"
    :pull-key "H-E"
    ;; :cmd "~/git/emacs/src/emacs"
    ;; :cmd "emacsclient --create-frame"
    :classes emacs-classes)

(defun first-existing-file (&rest files)
  (loop for file in files thereis
       (and (probe-file (parse-namestring file))
	    file)))

(defun first-existing-command (&rest commands)
  "assume command has no spaces or funny characters"
  (->> (run-shell-command (format nil "which ~{~A~^ ~}" commands) t)
       (cl-ppcre:split #\Newline)
       (car)))

(let ((eclipse-cmd
       (first-existing-command
        "eclipse"
        "android-studio"
        "STS")))

  (when eclipse-cmd
    (define-run-or-pull-program "android-studio"
        :classes '("jetbrains-studio" "Spring Tool Suite" "Eclipse")
        :cmd eclipse-cmd
        :raise-key "H-r")))

(define-run-or-pull-program "linphone"
    :raise-key "H-q"
    :pull-key "H-Q"
    :classes '("Linphone" "linphone"))

(define-run-or-pull-program "zathura")

(define-run-or-pull-program "vncviewer")



;; Warning: these bindings affect the *top-map*, which is
;; later deep-copied onto other per-window bindings.
;; Changes won't take effect on existing deep-copies of
;; *top-map*
;; for changes to take effect, 'per-window-bindings' and
;; top-map-bindings.lisp should be evaled in that order
