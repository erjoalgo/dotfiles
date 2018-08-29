(defvar *vocab-fn* (merge-pathnames "sensitive/vocab" STUMPWM-TOP))

(defun is-browser-win (win)
  (member (window-class win) browser-classes :test 'equal))

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
  (let* ((info (battery-info))
         (percentage (cdr (assoc :PERCENTAGE info)))
         (state (cdr (assoc :STATE info)))
         (time-left (cdr (or (assoc :|TIME TO FULL| info)
                             (assoc :|TIME TO EMPTY| info))))
	 (time (time-format *time-format-international*)))
    (echo-string-list
     (current-screen)
     (list
      time
      (format nil  "battery: ~A (~{~A~^, ~})" percentage
              `(,state ,@(when time-left (list (concat time-left " left")))))))))

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

(defcommand echo-pointer ()     ()
  "echo the mouse location"
  (echo (run-shell-command "xdotool getmouselocation" t)))

(defvar magnifier-on nil)

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
  (run-shell-command "wifi-connect"))

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

(defcommand spell-clipboard () ()
            "spell clipboard contents, character by character"
            (speak-string
             (cl-ppcre:regex-replace-all "(.)" (GET-X-SELECTION) " \\1")))

(defcommand echo-window-class () ()
  "echo window class"
  (message "window class: ~A" (window-class (current-window))))

(defcommand echo-current-tab () ()
  (echo (url-launcher-get-browser-current-url)))

(define-stumpwm-type :lisp-source-file (input prompt)
  (or (argument-pop input)
      (let ((basename
              (completing-read (current-screen)
                       (or "prompt: " "enter lisp file: ")
                       (mapcar #'pathname-name
                               (directory (merge-pathnames (make-pathname :type "lisp" :name :WILD) STUMPWM-TOP)))
                       :require-match t)))
        (merge-pathnames (make-pathname :name basename
                                        :type "lisp")
                         STUMPWM-TOP))
      (throw 'error "Abort.")))

(defcommand load-file (pathname) ((:lisp-source-file "enter lisp file to load: " ))
  "load a file"
  (in-package :stumpwm)
  (when pathname
    (load-safe pathname)))
