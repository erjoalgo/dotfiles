(setq *timeout-wait* 999)
(set-font "-*-*-bold-r-*-*-25-240-*-*-*-*-*-*")
(set-font "-adobe-helvetica-medium-r-normal--25-240-*-*-*-*-*-*")
(setq *message-window-gravity* :center)
(setq *input-window-gravity* :center)
(define-key *input-map* (kbd "C-v") 'input-yank-selection)
(setq *run-or-raise-all-groups* nil )
(setq *time-format-international* "%a %e %b %k:%M:%S")



;;(run-shell-command (format nil "display -window root ~A" (join-path HOME "sem/meta/2015-01-23-223306_1280x800_scrot.png")))
(setq *background-image-fn*
      (merge-pathnames  ".background-image-symlink" (user-homedir-pathname)))
;;(setq *background-image-fn* "/home/ealfonso/pictures/phone/Photaf-12-08-15_04-50-22/PhotafPanoramaPicHD.jpg")
(if (not *background-image-fn*)
    (print (concat "no background image found at" *background-image-fn*))
    (run-shell-command
     (format nil
	     "display -window root ~A"
	     *background-image-fn*))
    )
