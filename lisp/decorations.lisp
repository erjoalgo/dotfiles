(setq *timeout-wait* 999)
(set-font "-*-*-bold-r-*-*-25-240-*-*-*-*-*-*")
(set-font "-adobe-helvetica-medium-r-normal--25-240-*-*-*-*-*-*")
(setq *message-window-gravity* :center)
(setq *input-window-gravity* :center)
(define-key *input-map* (kbd "C-v") 'input-yank-selection)
(setq *run-or-raise-all-groups* nil )




(defvar *background-image-fn*
      (merge-pathnames  ".background-image-symlink" (user-homedir-pathname)))
(if (not *background-image-fn*)
    (print (concat "no background image found at" *background-image-fn*))
    (run-shell-command
     (format nil
	     "display -window root ~A"
	     *background-image-fn*)))
