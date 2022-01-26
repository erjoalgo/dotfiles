(in-package :STUMPWM)

(defun background-image-init ()
  (if (not *background-image-fn*)
      (print (concat "no background image found at" *background-image-fn*))
      (run-shell-command
       (format nil
	       "display -window root ~A"
	       *background-image-fn*))))
