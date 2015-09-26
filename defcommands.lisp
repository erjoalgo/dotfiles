(defcommand scroll-browser-other-frame (updown) ((:rest "key: "))
  (let* (
	 (currwin (current-window))
	 (visible-weasels
	  (filter-windows
	   (lambda (win)
	     (and
	      (window-visible-p win)
	      (gethash (window-class win) *browser-classes-hash*)
	      (not (eq win currwin))
	      )
	     )))
	 )
    (if (not visible-weasels)
	(echo "no visible browsers in other frames")
	(progn
	  (echo "sending key")
	  (assert (car visible-weasels))
	  (if (equal "Evince" (window-class (car visible-weasels)))
	      ;(setq updown (if (equal "d" updown) "Down" "Up")))
	      ;(setq updown (if (equal "d" updown) "SunPageUp" "SunPageDown")))
	      (setq updown (if (equal "d" updown) "n" "p")))
	  (message (format nil "sending ~A to ~A" updown (car visible-weasels)))
	  ;(utl-send-key (kbd updown) (car visible-weasels))
	  (send-meta-key-to-window (car visible-weasels) updown)
	  )
	)
    )
  )

(defcommand echo-date-battery () ()
  (let* (
	 (output (run-shell-command "battery_info.sh" t))
	 (percentage (extract-match "percentage:.*?([0-9][^%]*)%" output 1))
	 (state (extract-match "state:[^a-z]*([a-z-]+)" output 1))
	 (to-full (extract-match "time to (full|empty: *.+?)\\n" output 1))
	 (time (time-format *time-format-international*))
	 )
    
    ;(message "~a battery: ~a" time battery)
    ;(echo-string-list (current-screen) '((format nil "~a" "tiempo") ))
    (echo-string-list
     (current-screen)
     (list
      time
      ;;(concat "battery: " )
      ;;(format nil  "battery: ~A (~A, time to ~A)" percentage state to-full )
      (format nil  "battery: ~A (~A)" percentage state )
      ;;(and to-full (concat "time to " ))
      ))
    )
  )

(defcommand kill-window-by-class  ()   ()
  (let* (
	 (classes (mapcar 'window-class (screen-windows (current-screen))))
	 (to-kill (completing-read (current-screen) "enter a window class to kill (completing read!): " classes :require-match t))
	 )
    (when to-kill
      (mapcar 'kill-window (filter-windows (compose (curry 'equal to-kill) 'window-class))))))

(defcommand rotate-screen (orientation) ((:string "enter screen orientation: "))
  (run-shell-command (format nil "xrandr --orientation ~A; keynav_restart" orientation))
  )

(defcommand move-window-toggle () ()
  (move-window  (if (neighbour :right (tile-group-current-frame (current-group)) (group-frames (current-group))) :right :left))
  )

(defcommand search-engine-search (engine term)
    ((:string "enter search engine to use: ")
     (:string "enter search terms: "))
  (let* (
	 (escaped (escape-bash-single-quotes term))
	 )
    (and term
	 (progn 
	 ;(run-shell-command (format nil "~a  '~a'&" engine escaped))
	 (run-shell-command (format nil "~a  '~a'&" engine escaped))
	 (run-shell-command (format nil "echo '~a' >> ~~/search-history"
				    (join (coerce '(#\Tab) 'string)
					  engine term  (time-date-and-time))))
	 )
	 )
    )
  )

(defcommand cat-message-command (fn)
  ((:string "file to cat: "))
  (message (run-shell-command (format nil "cat ~a" fn) t))
  )

(defcommand dict-lookup-command (word)
  ((:string "Word to lookup: "))
  (let* (
	(definition
	 (run-shell-command (format nil "dict ~a" word) t))
	(*suppress-echo-timeout* t); let me read the definition in peace!
	)
    ;(message "~a is your word!" word)
    
  ;sed '/^NIL/d' ~/vocab 
    (and word
	 (message "definition: ~a" definition)
	 (run-shell-command (format nil "echo '~a' >> ~/vocab" word)))
    )
  )



(defcommand echo-current-group-name () ()
  (echo (prin1-to-string (group-name (current-group)))))

(defcommand invert-current-window () ()
  (let* (
	 (win (current-window))
	 (msg nil )
	 )
    (if (contains win inverted-windows)
	(progn
	    (remove-from-list 'inverted-windows win )
	    (setq msg "removing from inverted"))
	(progn
	  (add-to-list 'inverted-windows win )
	  (setq msg "adding to inverted"))
	)
    (invert-screen)
    (echo (concat msg (prin1-to-string win)))
    )
  )

(defcommand echo_pointer ()     ()
  (echo (run-shell-command "xdotool getmouselocation" t)))


(defcommand toggle-magnifier () ()
  (toggle-var magnifier-on)
  (run-shell-command
   (if (not magnifier-on)
      "magnifier -vm -z 5"
      "pkill magnifier")
      )
  )


(defcommand insert-key-with-delay (key &optional (times 1)) ((:string ) (:number))
  ;;(echo-format "times is ~D" times)
  ;;(sleep .001)
  (let* ((args
	  (if (eq times 1)
		key
		(reduce (lambda (cum b) (concat cum " " b))
			(loop for n from 0 below 3 collect key))
		)
	   ))
    (xdotool (concat "key " args)))
  )


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

(setq is-inverted-p nil )
(defcommand invert-screen () ()
  (toggle-var is-inverted-p)
  (run-shell-command "xcalib -a -i"))




;;based on 'echo-windows'
(defcommand my-echo-windows (&optional (fmt *window-format*) (group (current-group)) (windows (group-windows group))) (:rest)
  "Display a list of managed windows. The optional argument @var{fmt} can
be used to override the default window formatting."
  (let* ((wins (sort1 windows '< :key 'window-number))
         (highlight (position (group-current-window group) wins))
         (names (mapcar (lambda (w)
                          (format-expand *window-formatters* fmt w)) wins))
	 (group-info (concat "group: " (group-name (current-group))))
	 )
    (if (null wins)
        (echo-string-list (group-screen group) (list group-info "No Managed Windows"))
        (echo-string-list (group-screen group) (cons group-info names)
			  (and highlight (1+ highlight))))))

(defcommand connect-internet () ()
  (echo "connecting to internet...")
  (run-shell-command "sudo wifi -y -t ac&")
  )

    
(defcommand my-emacs-cmd () ()
  (let* (
	 (group (group-name (current-group)))
	 (cmd (format nil "emacs --stumpwm-group ~A" group))
	 )
    (run-or-raise cmd '(:class "Emacs"))
    )
  )
