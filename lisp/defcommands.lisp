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

;;based on 'echo-windows'
(defcommand echo-windows-with-group () ()
  (with-message-queuing t
    (message (concat "group: " (group-name (current-group))))
    (echo-windows)))

(defcommand connect-internet () ()
  "connect to the internet via the wifi-connect program"
  (echo "connecting to internet...")
  (run-command-async-notify "wifi-connect"))

(defcommand echo-window-class () ()
  "echo window class"
  (message "window class: ~A" (window-class (current-window))))

(defcommand echo-current-tab () ()
  (echo (url-launcher-get-browser-current-url)))

(define-stumpwm-type-from-wild-pathname :lisp-source-file
    (merge-pathnames (make-pathname :type "lisp" :name :WILD) STUMPWM-TOP))

(defcommand load-file (pathname) ((:lisp-source-file "enter lisp file to load: " ))
  "load a file"
  (in-package :stumpwm)
  (when pathname
    (load-safe pathname)))
