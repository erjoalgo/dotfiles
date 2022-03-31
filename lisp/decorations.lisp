(in-package :STUMPWM)

(defvar *background-image-filename* #P"~/.background-image-symlink")

(defun background-image-init ()
  (if (not *background-image-fn*)
      (message "No background image found at ~A" *background-image-filename*)
      (run-shell-command
       (format nil
	       "display -window root ~A"
	       *background-image-filename*))))
