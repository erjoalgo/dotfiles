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

(defun raise-pull-or-run-win (win-classes cmd-line raise-type &optional all-screens)
  (let* ((win-list (if all-screens (screen-windows (current-screen))
		       (group-windows (current-group))))
	 (curr-win (current-window))
	 (win-matches (lambda (win)
			(when win
                          (member (string-downcase (window-class win))
				  win-classes :test #'equal))))
         (curr-screen-matches-p (funcall win-matches curr-win))
	 (cands (remove-if-not win-matches win-list))
         (cands (sort cands #'< :key (lambda (win)
                                       (position (string-downcase (window-class win))
                                                 win-classes :test #'equal))))
	 (cand-no-curr (car (remove curr-win cands)))
         cmd-list)
    (when (and (null cand-no-curr) cmd-line (not curr-screen-matches-p))
      (setf raise-type :run))
    (ecase raise-type
      (:raise (when cand-no-curr
                (raise-window cand-no-curr)
                (focus-all cand-no-curr)))
      (:pull (when cand-no-curr
               (pull-window cand-no-curr)
               (focus-all cand-no-curr)))
      (:run (when cmd-line
              (setf cmd-list
                    (when cmd-line
                      (cond ((listp cmd-line) cmd-line)
                            ((stringp cmd-line) (ppcre:split " +" cmd-line))
                            ((functionp cmd-line) (funcall cmd-line)))))
              (destructuring-bind (command . args)
                  (loop for arg in cmd-list
                        collect
                        (if (pathnamep arg)
                            (uiop:native-namestring arg)
                            arg))
                (let ((proc (start-porcess-with-logging command args)))
                  (when raise-window-in-original-group-secs
                    (push (cons (sb-ext:process-pid proc)
                                (cons (current-group) (GET-UNIVERSAL-TIME)))
                          pid-original-group-alist)))))))))

(defmacro define-run-or-pull-program (name
				      &key
					(raise-key (format nil "H-~A" (char name 0)))
					(pull-key (string-upcase raise-key))
					(run-key nil)
					(cmd name)
					(classes `(list ,(string-capitalize name)))
                                        (all-screens nil)
                                        (keymap '*top-map*))

  `(progn
     ,@(loop for (raise-type key) in `((:raise ,raise-key)
				       (:pull ,pull-key)
                                       (:run ,run-key))
	     as cmd-name = (format nil "~A-~A"
                                   (symbol-name raise-type)
                                   (string-upcase name))
	     as doc = (format nil "~A '~A' (autogenerated)" raise-type cmd)
	     collect
             `(defcommand ,(intern cmd-name) () ()
                ,doc
		(raise-pull-or-run-win
                 (mapcar 'string-downcase ,classes)
		 ,cmd ,raise-type ,all-screens))
	     when key collect
	       `(define-key-path ,keymap ,key ,cmd-name))))

(defun define-key-path (keymap key-path cmd)
  (loop
    with curr-kmap = (or keymap *top-map*)
    for keys-left on (ppcre:split " " key-path)
    as key = (car keys-left)
    as is-last = (null (cdr keys-left))
    as kbd = (kbd key)
    as val = (lookup-key curr-kmap kbd)
    do (progn
         (when (symbolp val)
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
