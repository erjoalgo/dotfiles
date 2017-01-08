(defvar *vol-muted* nil)
(defvar *VOLSTEP* 3)

(defcommand volchange (up percentage)
    ((car (:y-or-n "raise volume?"))
     (:number "percentage?"))
  "change volume by a percentage, via amixer"
    (let* (
	   (sign (if up "+" "-"))
           (cmd (format nil
			"amixer set Master ~D%~A"
			percentage
			sign))
	   )
      ;;(echo (format nil "running: ~A" cmd))
      (run-shell-command cmd)
      (if up (setq *vol-muted* nil ))
      ))
;;(run-shell-command "amixer set Master 0%")
(defun get-curr-volume ()
  (let* (
         (out (run-shell-command "amixer get Master" t))
         ;;(vol (extract-match "'Master',([0-9]+)" out 1))
         (vol (extract-match "Playback.*?[[]([0-9]+)%]" out 1))
         )
    vol
    )
  )

(defcommand volmute-toggle ()()
  "mute/unmute"
  (if (eq *vol-muted* nil )
      (progn
        (setq *vol-muted* (get-curr-volume))
        (run-shell-command "amixer set Master 0%"))
      (run-shell-command (format nil
                                 "amixer set Master ~A%"
                                 *vol-muted*)))

  )



(defcommand volup ()() "volume up" (volchange t *VOLSTEP* ))
(defcommand voldown ()() "volume down" (volchange nil *VOLSTEP* ))




