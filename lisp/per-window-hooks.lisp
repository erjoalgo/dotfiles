;;TODO
(defun add-window-specific-hook (window-pred into-hook out-hook)
  "run into-hook, out-hook before switching into, out-of any window matching window-pred"
  (let ((fun (lambda (old-win new-win)
	       (when (funcall window-pred old-win)
		 (echo "calling into hook" )
		 (funcall out-hook old-win))

	       (when (funcall window-pred new-win)
		 (echo "calling into hook" )
		 (funcall into-hook new-win)))))

    (add-hook STUMPWM::*focus-window-hook* fun)))


