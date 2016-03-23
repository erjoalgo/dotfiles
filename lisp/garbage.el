(defcommand asdfasdf () ()
  (run-shell-command (format nil "sleep .5; xdotool type '~A'; xdotool key Return" cmu))
  ;(run-shell-command "xdotool key Return")
  ;(run-shell-command "sleep .5; xdotool key Return")
  )

;;(fset regular-file-lister (compose (compose (curry 'remove-if 'not) (curry 'mapcar 'pathname-name)) 'list-directory) )


(defun any (func L)
  (and L (or (funcall func (car L)) (any func (cdr L))))
  )

(defun contains (elm L)
  (reduce (lambda (cum el) (or cum (equal el elm))) L :initial-value nil ))

(defun contains (elm L)
  (and L (or (equal (car L) elm) (contains elm (cdr L)))))

(setq inverted-windows nil )




(defun maybe-invert-rising-window (new old)
  (if (and
       (not (equal (member new inverted-windows)
		   (member old inverted-windows)))
       ;(not is-inverted-p)
       t
       )
      (progn
	;(echo "inverting")
	(invert-screen))))

(defun display-is-inverted ()
  (let* ((out (run-shell-command "xrandr -q | grep ' connected.*inverted'" t)))))
(defcommand cat-message-command (fn)
  ((:string "file to cat: "))
  (message (run-shell-command (format nil "cat ~a" fn) t)))

(defcommand echo-current-group-name () ()
  (echo (prin1-to-string (group-name (current-group)))))
(toggle-var is-inverted-p)
(defcommand xev ()
   ()
  (|autogen-raise-x-terminal-emulator|)
  ;(raise-x-terminal-emulator)
  (only)
  ;;(xdotool "key ctrl+a ctrl+k")
  ;;(xdotool "type 'xev|grep keycode'")
  ;(xdotool "type xev|grep keycode"); > /tmp/xev" )
  (xdotool "key Return" )
  (vsplit)
  '(add-hook *focus-window-hook-one-time*
	    (lambda ()
	      (fnext)
	      (|pull-x-terminal-emulator|)
	      (select-window-by-class nil (lambda (win) (equal (window-title win) "Event Tester")))
	      ))
  )
(defcommand insert-key-with-delay (key &optional (times 1))
  ((:string ) (:number))
  (let* ((args
	  (if (eq times 1)
	      key
	    (reduce (lambda (cum b) (concat cum " " b))
		    (loop for n from 0 below 3 collect key)))))
    (xdotool (concat "key " args))))





(setq is-inverted-p nil )
(defcommand invert-screen () ()
  (toggle-var is-inverted-p)
  (run-shell-command "xcalib -a -i"))
(defcommand invert-current-window () ()
  (let* (
	 (win (current-window))
	 (msg nil )
	 )
    (if (contains win inverted-windows)
	(progn
	    (remove win inverted-windows)
	    (setq msg "removing from inverted"))
	(progn
	  (add-to-list 'inverted-windows win )
	  (setq msg "adding to inverted"))
	)
    (invert-screen)
    (echo (concat msg (prin1-to-string win)))
    )
  )
(defcommand my-emacs-cmd () ()
  (let* ((group (group-name (current-group)))
	 (cmd (format nil "emacs --stumpwm-group ~A" group)))
    ;;(run-or-raise cmd '(:class "Emacs"))
    (let ((matches (find-matching-windows '(:class "Emacs") nil t)))
      (when matches (focus-all (car matches))))))

(defcommand emacs-in-group () ()
  (let* ((group (group-name (current-group)))
	 ;;(cmd (format nil "emacs --stumpwm-group ~A" group)))
	 (cmd "emacs"))
    (run-shell-command cmd nil )))

