;;only defuns
(defun hide-message-windows ()
  (when (timer-p *message-window-timer*)
            (cancel-timer *message-window-timer*)
            (setf *message-window-timer* nil))
    (progn (unmap-all-message-windows) (unmap-all-message-windows)))


(defun filter-windows (pred)
  (remove-if-not pred (screen-windows (current-screen))))

(defun xdotool (cmd)
  (run-shell-command (format nil "xdotool ~A" cmd) t))
