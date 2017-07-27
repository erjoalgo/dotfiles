
(defvar *vocab-fn* (stumpwm-merger "sensitive/vocab"))

(defun is-browser-win (win)
  (member (window-class win) *browser-classes* :test 'equal))

(defcommand other-frame-scroll-browser (up-down-key)
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
	 (output (run-shell-command "battery-info.sh" t))
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
  (mapcar 'kill-window
	  (remove-if-not
	   (lambda (win)
	     (equal
	      (window-class win)
	      win-class))
	   (screen-windows (current-screen)))))

(define-stumpwm-type-for-completion
    :xrandr-rot
    '("left" "right" "normal" "inverted"))

(defcommand rotate-screen (orientation)
    ((:xrandr-rot "enter xrandr orientation: "))
  "rotate screen"
  (let ((cmd (format nil
		     "xrandr --output VGA1 --orientation ~A"
		     orientation)))
    (run-shell-command cmd t)
    cmd))


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
  "toogle magnifier on/off. requires the TODO program"
  (setf magnifier-on (not magnifier-on))
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
  (merge-pathnames  "pictures/auto-scrots/" (user-homedir-pathname)))





(defcommand scrot-cmd (name)
    ((:string "enter name for scrot: "))
  "save a scrot to *scrots-top*"
  (ensure-directories-exist *scrots-top*)
  ;;TODO allow selecting region
  (unmap-message-window (current-screen))
  (sleep 1)
  (let ((out-png (namestring (merge-pathnames (make-pathname :name name :type "png")
				   *scrots-top*))))

    (SB-EXT:RUN-PROGRAM "shutter"
			(list "-e" "-f" "-o" out-png)
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

(defcommand speak-key ()
    ;;((:key "enter key to speak: " ));;this requires pressing enter
    ()
  ;;(let ((text (key-keysym key)))
  "speak the name of the next typed key"
  (let ((text (read-one-char (current-screen))))
    (SB-EXT:RUN-PROGRAM "espeak"
			(list (format nil "~A" text))
			:search t
			:output t
			:error t
			:wait t)))

(defcommand speak-string (text)
    ((:string "enter string to speak: " ))
  "speak some text"
  (SB-EXT:RUN-PROGRAM "espeak"
			(list text)
			:search t
			:output t
			:error t
			:wait t))

(defcommand echo-window-class () ()
  "echo window class"
  (message "window class: ~A" (window-class (current-window))))

'(defcommand send-keys (&rest keys) ((:key-seq "enter keys"))
  "send the specified keys into the current screen"
  (echo keys)
  '(mapcar (lambda (key) (send-meta-key (current-screen) (kbd key)))
	  keys))
'(define-key *top-map* (kbd "F12") "SEND-KEYS TAB END RET" )

(defcommand type-ge-email
    () ()
  (run-shell-command
   "sleep 1 && xdotool type ernesto.alfonsogonzalez@ge.com"))

(defcommand type-gmail
    () ()
  (run-shell-command
   "sleep 1 && xdotool type erjoalgo@gmail.com"))

(defcommand type-clipboard-contents () ()
  (let* ((clipboard (get-x-selection)))
    (run-shell-command (format nil "xdotool type ~A" clipboard))))
