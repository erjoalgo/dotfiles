(push '(:class "VirtualBox") *deny-raise-request*)
(push '(:class "Pidgin") *deny-raise-request*)
;;(push '(:class "Xterm") stumpwm:*deny-raise-request*)
;;(add-hook *focus-window-hook* 'maybe-invert-rising-window)
(add-hook *focus-window-hook*
	  (lambda (focuswin old)
	    (let* ((old-hooks *focus-window-hook-one-time*))
	      (setq *focus-window-hook-one-time* nil )
	    (mapc 'funcall old-hooks))))
