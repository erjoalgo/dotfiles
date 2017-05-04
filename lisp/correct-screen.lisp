(defun run-shell-command-print (cmd &optional collect-output-p)
  (print cmd)
  (run-shell-command cmd collect-output-p))

(defstruct xrandr-display id state mode modes connected-p extra)

(defun xrandr-displays ()
  ;; return a list of xrandr-display
  (loop
     with lines = (cdr (ppcre:split #\Newline
				    (run-shell-command "xrandr -q" t)))
     with pop-line = (lambda () (ppcre:split " +" (pop lines)))
     while lines
     collect (destructuring-bind (id state . etc) (funcall pop-line)
	       (declare (ignore etc))
	       (let ((modes (loop
			       while (ppcre:scan "^ +[0-9]+x[0-9]+" (car lines))
			       collect (cdr (funcall pop-line))))
		     (extra (loop
			       while (ppcre:scan "^ +" (car lines))
			       collect (funcall pop-line))))
		 (make-xrandr-display :id id :state state
				      :modes (mapcar (lambda (mode)
						       (ppcre:register-groups-bind (w h)
							   ("([0-9]+)x([0-9]+)i?"
							    (car mode))
							 (mapcar 'parse-integer (list w h))))
						     modes)
				      :connected-p (equal state "connected")
				      :extra extra)))))

(defun correct-screen (&optional order)
  (let* ((displays (xrandr-displays))
	 (connected (remove-if-not 'xrandr-display-connected-p
				   displays))
	 (to-connect-ordered (if order (loop for ith in order
					  collect (nth ith connected))
				 connected))
	 (to-disconnect (remove-if (lambda (display)
				     (member display to-connect-ordered))
				   displays)))
    ;; disconnect
    (mapc (lambda (off-display)
	    (run-shell-command-print
	     (format nil "xrandr --output ~A --off"
		     (xrandr-display-id off-display))))
	  to-disconnect)
    ;; connect displays in order
    (loop for display in to-connect-ordered
       with pos-x = 0
       as mode = (car (xrandr-display-modes display))
       do (destructuring-bind (mode-width mode-height) mode
	    (declare (ignore mode-height))
	    (let* ((id (xrandr-display-id display))
		   (mode-string (format nil "~{~a~^x~}" mode))
		   (pos-string (format nil "~Dx0" pos-x))
		   (cmd (format nil "xrandr --output ~A --mode ~A --pos ~A"
				id mode-string pos-string )))
	      (run-shell-command-print cmd))
	    (incf pos-x mode-width)))))

(defcommand correct-screen-prompt-display-order () ()
  "correct screen, prompting for display order"
  (let* ((displays (remove-if-not
		    'xrandr-display-connected-p
		    (xrandr-displays)))
	  (prompt (format nil "~{~{~a: \"~a\"~}~^, ~}: "
			  (loop for display in (mapcar 'xrandr-display-id displays)
			     for i from 0
			     collect (list i display))))
	   ;(line (read-one-line (current-screen) prompt))
	   ;(line (progn (echo (mapcar 'caar info))
			;(read-one-line (current-screen) ": ")))
	   (line (if (null (cdr displays))
		     ;; `((0 . ,(car displays)))
		     "0"
		     (read-one-line (current-screen)
				prompt)))
	   (order (loop for c across line
		     collect (- (char-code c) (char-code #\0))))
	   )
    (when (and order
	       (>= (apply 'max order) (length displays)))
	(error "index out of bounds"))
      (correct-screen order)))

'(when display-names
  (if (null (cdr display-names))
      (car display-names)
      (let ((prompt (format nil "select display order: ~A~%"
			    (apply 'join #\Newline display-names)))
	    (response (completing-read (current-screen)
				       prompt
				       display-names :require-match t)))
	(cons (correct-screen-prompt-display-order)))))


