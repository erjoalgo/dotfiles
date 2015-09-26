;;only defuns

(defun exists-window-current-screen (props)
  (find-matching-windows props nil nil )
  )

(defun get_window_id (regexp)  )

(defun hide-message-windows ()
  (when (timer-p *message-window-timer*)
            (cancel-timer *message-window-timer*)
            (setf *message-window-timer* nil))
    (progn (unmap-all-message-windows) (unmap-all-message-windows))
  )


(defun select-window-by-class (class &optional fun)
  (when class (setq class (string-downcase class)))
  (setq fun (or fun (lambda (win) (equal (string-downcase (window-class win)) class))))
  (let (
	(matching (filter-windows (lambda (win) (funcall fun win)) ))
	)
    (when matching  (select-window-by-number (window-number (car matching))))
   )
  )

(defun remove-from-list (list-var elm)
  (set list-var (remove-if (lambda (a) (equal a elm)) (symbol-value list-var))))

(defun switch-to-window (win &optional no_frame_indicator)
  (assert win)
  (let* ((group (window-group win))
	 (frame (window-frame win))
	 (old-frame (tile-group-current-frame group)))
    (focus-all win)
    ;add kw arg to prevent this
    (if (and (not no_frame_indicator) (eq frame old-frame))
             (show-frame-indicator group)
             (echo "")
	     ))
  )

(defun send-meta-key-to-window (win key)
  (send-fake-key win (kbd key)); does not work
  '(let* ((old-window (current-window)))
    (switch-to-window win t)
    (send-meta-key (current-screen) (kbd key))
    (switch-to-window old-window t)
    )
  )

(defun filter-windows (fun &optional all-screens)
  ;(remove nil (loop for win in (screen-windows (current-screen)) collect (when (equal (window-class win) "Emacs") win) ))
  (remove nil (loop for win in (screen-windows (current-screen)) collect
		   (when
		       (funcall fun win)
		     win)
		   ))
  ;(loop for win in (screen-windows (current-screen)) (when (funcall fun win) collect win))
  )

(defun xdotool (cmd)
  (run-shell-command (format nil "xdotool ~A" cmd) t))


'(defun current-window ()
  (tile-group-current-frame (current-group))
  )
