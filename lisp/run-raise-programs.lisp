(in-package :STUMPWM)

(defvar pid-original-group-alist nil
  "An alist (PID . (GROUP . TIMESTAMP)) that records
the group/workspace in which a process was originally started")

(defparameter raise-window-in-original-group-secs 10
  "If non-nil, any window from a process originally
started in group A that is raised in group B
is moved back to group A if its process
was started less than RAISE-WINDOW-IN-ORIGINAL-GROUP-SECS
seconds ago")

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

(defun raise-pull-or-run-win (win-classes cmd-line &optional pull-p all-screens)
  (let* ((win-list (if all-screens (screen-windows (current-screen))
		       (group-windows (current-group))))
	 (curr-win (current-window))
	 (win-matches (lambda (win)
			(member (string-downcase (window-class win))
				win-classes :test #'equal)))
	 (cands (remove-if-not win-matches win-list))
         (cands (sort cands #'< :key (lambda (win)
                                       (position (string-downcase (window-class win))
                                                 win-classes :test #'equal))))
	 (cand-no-curr (car (remove curr-win cands)))
         (cmd-list (ppcre:split " +" cmd-line)))
    (if cand-no-curr
	(progn (funcall (if pull-p 'pull-window
			    'raise-window)
			cand-no-curr)
	       (focus-all cand-no-curr))
	(unless (and curr-win
		     (funcall win-matches curr-win))
          (destructuring-bind (command . args) cmd-list
	    (let* ((log-file (merge-pathnames #P"/tmp/"
                                              (make-pathname
                                               :name (pathname-name command)
                                               :type "log")))
                   (proc
                    ;; this creates an extra shell whose pid doesn't match window pid
                    ;; (run-shell-command command)
                    (with-open-file
                        (out-fn log-file
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
                      (sb-ext:run-program
                       command args
                       :wait nil
                       :search t
                       :output out-fn
                       :if-output-exists :append))))
              (when raise-window-in-original-group-secs
                (push (cons (sb-ext:process-pid proc)
                            (cons (current-group) (GET-UNIVERSAL-TIME)))
                      pid-original-group-alist))))))))

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

	     as cmd-name = (intern
                            (format nil "~A-~A"
                                    (symbol-name pull-or-raise-fun)
                                    (string-upcase name)))
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
		 `(define-key-path ,keymap ,key ,cmd-name-string))))))

(defun define-key-path (keymap key-path cmd)
  (loop
    with curr-kmap = (or keymap *top-map*)
    for keys-left on (ppcre:split " " key-path)
    as key = (car keys-left)
    as is-last = (null (cdr keys-left))
    as kbd = (kbd key)
    as val = (lookup-key curr-kmap kbd)
    do (progn
         (when (kmap-or-kmap-symbol-p val)
           (setf val (symbol-value val)))
         (if (not is-last)
             (progn
               (if val
                   (assert (kmap-p val))
                   (progn
                     (setf val (make-sparse-keymap))
                     (define-key curr-kmap kbd val)))
               (setf curr-kmap val))
             (progn
               (assert (not (kmap-p val)))
               (define-key curr-kmap kbd cmd))))))
