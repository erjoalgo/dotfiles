(defvar *browser-classes-hash* nil )
(defvar *vocab-fn* (stumpwm-merger "sensitive/vocab"))

(defun is-browser-win (win)
  (gethash (window-class win) *browser-classes-hash*))

(defcommand scroll-browser-other-frame (up-down-key)
    ((:rest "key: "))
  "scroll up/down the browser which is on another, visible frame"
  (let* ((curr-win (current-window))
	 (visible-browser-wins
	  (remove-if-not
	   (lambda (win)
	     (and
	      (not (eq win curr-win))
	      (window-visible-p win)
	      (is-browser-win win)))
	   (screen-windows (current-screen)))))
    
    (when visible-browser-wins
      (send-fake-key (car visible-browser-wins) (kbd up-down-key)))))

(defvar *time-format-international* "%a %e %b %k:%M:%S")
(defcommand echo-date-battery () ()
  "echo date and battery status"
  (let* (
	 (output (run-shell-command "battery_info.sh" t))
	 (percentage (extract-match "percentage:.*?([0-9][^%]*)%" output 1))
	 (state (extract-match "state:[^a-z]*([a-z-]+)" output 1))
	 ;(to-full (extract-match "time to (full|empty: *.+?)\\n" output 1))
	 (time (time-format *time-format-international*))
	 )
    (echo-string-list
     (current-screen)
     (list
      time
      (format nil  "battery: ~A (~A)" percentage state )))))

(define-stumpwm-type-for-completion
    :win-class
    (mapcar 'window-class (screen-windows (current-screen))))

(defcommand kill-window-by-class (win-class)
    ((:win-class "enter window class to kill: "))
  "kill window by class"
  ;;so silly...
  (mapcar 'kill-window
	  (filter-windows
	   (compose
	    (curry 'equal win-class)
	    'window-class))))

(define-stumpwm-type-for-completion
    :xrandr-rot
    '("left" "right" "normal" "inverted"))

(defcommand rotate-screen (orientation)
    ((:xrandr-rot "enter xrandr orientation: "))
    "rotate screen"
  (run-shell-command (format nil "xrandr --orientation ~A; keynav_restart" orientation)))

(defcommand move-window-toggle () ()
  "move window to next monitor (display)"
  (move-window  (if (neighbour :right (tile-group-current-frame (current-group))
			       (group-frames (current-group))) :right :left)))

(defcommand dict-lookup-command (word)
    ((:string "Word to lookup: "))
  "lookup a word in the dictionary (requires the dict package)"
  (let* (
	(definition
	 (run-shell-command (format nil "dict ~a" word) t))
	(*suppress-echo-timeout* t); let me read the definition in peace
	)
    (when word
      (log-entry-timestamp word *vocab-fn*)
      (message "definition: ~a" definition))))







(defcommand echo_pointer ()     ()
  "echo the mouse location" 
  (echo (run-shell-command "xdotool getmouselocation" t)))


(defvar magnifier-on nil )
(defcommand toggle-magnifier () ()
  "toogle magnifier on/off. requires the magnifier package" 
  (toggle-var magnifier-on)
  (run-shell-command
   (if (not magnifier-on)
      "magnifier -vm -z 5"
      "pkill magnifier")))



(defcommand invert-screen () ()
  "invert screen"
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
  "connect to the internet via the wifi program"
  (echo "connecting to internet...")
  ;;debian-mini$kill-wpa-supplicants.sh ; sleep 1 && wac
  (run-shell-command "kill-wpa-supplicants.sh; sleep 1 && sudo wifi -y -t ac&"))

    


;;(emacs-in-group)

  
(defparameter *scrots-top*
  ;;TODO use expand user or some pathname library
  "/home/ealfonso/pictures/auto-scrots" )

(defcommand scrot-cmd (name)
    ((:string "enter name for scrot: "))
  "save a scrot to *scrots-top*"
  ;;TODO allow selecting region
  (let ((out-png (format nil 
			 "~A/~A.png" *scrots-top* name)))
    (SB-EXT:RUN-PROGRAM "scrot"
			(list out-png)
			:search t
			:output t
			:error t
			:wait t)
    ;;why does this crash the session
    '(SB-EXT:RUN-PROGRAM "eog"
			(list out-png)
			:search t)
    (set-x-selection out-png :clipboard)
    (message "copied to cliboard: ~A" out-png)))
