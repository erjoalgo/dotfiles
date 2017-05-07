;; (defpackage #:brightness)

(defmacro -> (&rest forms)
  (if (cadr forms)
      ;;(destructuring-bind (first (a a-rest) . rest) forms
      ;;`(-> a first a-rest ,@rest))
      (destructuring-bind (first second . rest) forms
	(destructuring-bind (a . a-rest) (if (atom second)
					     (cons second nil)
					   second)
	  `(-> ,(apply 'list a first a-rest) ,@rest)))
    (car forms)))

(defmacro ->> (&rest forms)
  (if (second forms)
      (destructuring-bind (a b . cde) forms
	(let ((b (if (atom b) (list b) b)))
	  `(->> ,(nconc b (list a)) ,@cde)))
    (first forms)))

(defvar *actual-brightness-pathname*)
(defvar *brightness-pathname*)
(defvar *max-brightness-pathname*)
(defvar *max-brightness*)
(defparameter *min-brightness* 3)

(defun init-brightness ()
  (let ((actual-brightness-pathname
	 (-> "find /sys/devices -name actual_brightness"
	     (run-shell-command t)
	     trim-spaces
	     pathname)))
    (setf *actual-brightness-pathname*
	  actual-brightness-pathname
	  *max-brightness-pathname*
	  (make-pathname :name "max_brightness" :defaults
			 *actual-brightness-pathname*)
	  *brightness-pathname*
	  (make-pathname :name "brightness" :defaults
			 *actual-brightness-pathname*)
	  *max-brightness* (read-brightness *max-brightness-pathname*))))

(defun read-brightness (pathname)
  (-> pathname
      read-file-into-string
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
    cmd))


(defun up-down-brightness (up-p)
  (let* ((current (read-brightness *actual-brightness-pathname*))
	 (percentage (/ (- current *min-brightness*)
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
  (up-down-brightness t))

(defcommand brightness-down () ()
  (up-down-brightness nil))

(init-brightness)
