;; (defpackage #:brightness)

(defvar *actual-brightness-pathname*)
(defvar *brightness-pathname*)
(defvar *max-brightness-pathname*)
(defvar *max-brightness*)
(defparameter *min-brightness* 3)

(defun run-shell-command-t (cmd)
  (run-shell-command cmd t))

(defun init-brightness ()
  (flet ((find-pathname (filename)
	   (->>
	    (format nil "find /sys/devices -name ~A"
		    filename)
	    run-shell-command-t
	    (cl-ppcre:split #\Newline)
	    car
	    trim-spaces
	    pathname)))
    (setf *actual-brightness-pathname*
	  (find-pathname "actual_brightness")
	  *max-brightness-pathname*
	  (make-pathname :name "max_brightness" :defaults
			 *actual-brightness-pathname*)
	  *brightness-pathname*
	  (make-pathname :name "brightness" :defaults
			 *actual-brightness-pathname*)
	  *max-brightness* (read-brightness *max-brightness-pathname*))))

(defun read-brightness (pathname)
  (-> pathname
      file-string
      trim-spaces
      parse-integer))

(defun set-brightness (percentage)
  (let* ((new-actual (->
		     (+ *min-brightness*
		       (->
			(- *max-brightness* *min-brightness*)
			(* percentage)))
		     round))
	(cmd (format nil "echo ~D | sudo tee ~A"
	     new-actual *brightness-pathname*)))
    (run-shell-command cmd t)
    (echo cmd)))


(defun up-down-brightness (up-p)
  (let* ((current (read-brightness *actual-brightness-pathname*))
	 (percentage (/ (max 0 (- current *min-brightness*))
			(- *max-brightness* *min-brightness*)))
	 (delta (if (zerop percentage)
		    0
		    (expt 10 (round (1- (log percentage 10))))))
	 (min-percentage-delta (/ 1 *max-brightness*))
	 (new-percentage (+ percentage
			    (* (max min-percentage-delta delta)
			       (if up-p 1 -1)))))
    (set-brightness new-percentage)
    (print new-percentage)))

(defcommand brightness-up () ()
  "increase brightness"
  (up-down-brightness t))

(defcommand brightness-down () ()
  "decrease brightness"
  (up-down-brightness nil))

(init-brightness)
