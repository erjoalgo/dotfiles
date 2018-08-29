(defvar *vol-muted* nil)
(defvar *VOLSTEP* 3)

(defcommand volchange (up percentage)
    ((car (:y-or-n "raise volume?"))
     (:number "percentage?"))
  "change volume by a percentage, via amixer"
  (let* ((sign (if up "+" "-"))
         (cmd (format nil
		      "amixer set Master ~D%~A"
		      percentage
		      sign)))
    ;;(echo (format nil "running: ~A" cmd))
    (run-shell-command cmd)
    (if up (setq *vol-muted* nil))))

(defun get-curr-volume ()
  (let* ((out (run-shell-command "amixer get Master" t))
         (vol (ppcre:register-groups-bind (vol) ("Playback.*?[[]([0-9]+)%]" out)
                vol)))
    vol))

(defcommand volmute-toggle ()()
  "mute/unmute"
  (if (null *vol-muted*)
      (progn
        (setq *vol-muted* (get-curr-volume))
        (run-shell-command "amixer set Master 0%"))
      (progn (run-shell-command (format nil
                                        "amixer set Master ~A%"
                                        *vol-muted*))
             (setf *vol-muted* nil))))

(defcommand volup ()() "volume up" (volchange t *VOLSTEP* ))
(defcommand voldown ()() "volume down" (volchange nil *VOLSTEP*))
