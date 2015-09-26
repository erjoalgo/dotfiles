(defun maybe-invert-rising-window (new old)
  (if (and
       (not (equal (contains new inverted-windows) (contains old inverted-windows)))
       ;(not is-inverted-p)
       t
       )
      (progn
	;(echo "inverting")
	(invert-screen))))

(defun display-is-inverted ()
  (let* ((out (run-shell-command "xrandr -q | grep ' connected.*inverted'" t)))))

(push '(:class "VirtualBox") *deny-raise-request*)
(push '(:class "Pidgin") *deny-raise-request*)
;;(push '(:class "Xterm") stumpwm:*deny-raise-request*)
;;(add-hook *focus-window-hook* 'maybe-invert-rising-window)
(setq inverted-windows nil )


(add-hook *focus-window-hook*
	  (lambda (focuswin old)
	    (let* ((old-hooks *focus-window-hook-one-time*))
	      (setq *focus-window-hook-one-time* nil )
	    (mapc 'funcall old-hooks))
	    
	    ))
