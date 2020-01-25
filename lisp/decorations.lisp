(in-package :STUMPWM)

(defparameter *font-size* 40)
(setq *timeout-wait* 999)
(define-key *input-map* (kbd "C-v") 'input-yank-selection)
(defvar *background-image-fn*
      (merge-pathnames  ".background-image-symlink" (user-homedir-pathname)))

(defun decorations-init ()
  (setq *message-window-gravity* :center)
  (setq *input-window-gravity* :center)
  (setq *run-or-raise-all-groups* nil)
  (set-font (format nil "-*-*-bold-r-*-*-~D-240-*-*-*-*-*-*"
		    *font-size*))
  (if (not *background-image-fn*)
      (print (concat "no background image found at" *background-image-fn*))
      (run-shell-command
       (format nil
	       "display -window root ~A"
	       *background-image-fn*))))
