(in-package :STUMPWM)

(defvar *vocab-fn* (merge-pathnames "sensitive/vocab" *stumpwm-top-directory*))

(define-stumpwm-type-from-wild-pathname :lisp-source-file
    (merge-pathnames (make-pathname :type "lisp" :name :WILD) *stumpwm-top-directory*))

(defun load-safe (pathname)
  "load file, trapping and recording errors"
  (handler-case
      (with-elapsed-time elapsed-time (load pathname)
        (message "loaded ~A in ~Dms" (pathname-name pathname) elapsed-time))
    (error (err)
      (message "error loading: ~A~%: '~A'" pathname err)
      (cons pathname err))))

(defcommand load-file (pathname) ((:lisp-source-file "enter lisp file to load: " ))
  "load a file"
  (in-package :stumpwm)
  (when pathname
    (load-safe pathname)))

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
      (log-entry-timestamped word *vocab-fn*)
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

(defcommand connect-to-internet () ()
  "connect to the internet via the wifi-connect program"
  (echo "connecting to the internet...")
  ;; never prompt
  (run-command-async-notify "wifi-connect" '("connect" "-Pnever")))

(defcommand connect-to-internet-maybe () ()
  "connect to the internet via the wifi-connect program"
  (if (connect-to-internet-connected-p)
      (echo "already connected to the internet")
      (connect-to-internet)))

(defcommand connect-to-internet-connected-p () ()
  "check if connected to the internet"
  (multiple-value-bind (retcode _output)
      (run-command-retcode-output "curl" '("ipecho.erjoalgo.com"
                                           "--max-time=10"
                                           "--connect-timeout=10"))
    (declare (ignore _output))
    (zerop retcode)))

(defcommand echo-window-class () ()
  "echo window class"
  (message "window class: ~A" (window-class (current-window))))

(defcommand echo-window-pid () ()
  "echo window class"
  (message "window pid: ~A" (window-pid (current-window))))

(defcommand echo-current-tab () ()
  (echo (url-launcher-get-browser-current-url)))

(define-stumpwm-type-from-wild-pathname :lisp-source-file
    (merge-pathnames (make-pathname :type "lisp" :name :WILD) *stumpwm-top-directory*))

(defcommand byzanz-record (name duration)
    ((:string " recording name: ")
     (:number "recording duration in seconds: "))
  (assert (which "byzanz-record"))
  (let* ((name (or name
                   (time-format *scrot-date-format*)))
         (recording-pathname
           (merge-pathnames (make-pathname
                             :name name
                             :type "gif")
                            *scrots-top*))
         (duration-args
           (if duration (list "-d" duration)
               (list "-e"
                     (format nil "nc -l ~A ~D"
                             "-p" ;; not always the same
                             byzanz-recording-control-port)))))
    (set-x-selection (namestring recording-pathname) :clipboard)
    (message "starting byzanz recording in 1s...")
    (sleep 1)
    (unmap-all-message-windows)
    (run-command-async-notify
     "byzanz-record"
     `(,@duration-args ,recording-pathname))))

(defvar byzanz-recording-control-port 17909)

(defcommand byzanz-record-auto () ()
  (handler-case (byzanz-record-auto-stop)
    (USOCKET:CONNECTION-REFUSED-ERROR (_ex)
      (declare (ignore _ex))
      ;; no recording in progress. start a new one...
      (byzanz-record nil nil))))

(defcommand byzanz-record-auto-stop () ()
  (mozrepl:nc "localhost" byzanz-recording-control-port "1")
  (sleep 1)
  (message "stopping byzanz recording..."))

(defcommand emacs-killusr2 () ()
  "invoke this command to debug an emacs hang/freeze"
  (run-command-async-notify
   "pkill"
   (list "-SIGUSR2" "emacs")))

(defcommand emacs-killusr2-tmux () ()
  "send the keybinding for tmux to invoke the emacs kill command"
  (run-command-async-notify
   "xdotool" (list
              "keyup" "Hyper_R"
              "keyup" "Shift_R"
              "key" "Control+b" "key" "C")))

(defcommand path-append-directory (directory)
    ;; ((:directory "enter directory to append to the path: "))
    ((:string "enter directory to append to the path: "))
  (assert (truename directory))
  (sb-posix:setenv "PATH"
                   (concatenate 'string (sb-posix:getenv "PATH") ":" directory)
                   1)
  (echo-string-list (current-screen)
                    (ppcre:split #\: (sb-posix:getenv "PATH"))))

(defcommand screen-lock () ()
  (run-shell-command (screen-lock-program)))

(defcommand window-sleep-toggle () ()
  (let* ((win (current-window))
         (pid (window-pid win))
         (state (process-state pid))
         (signal (if (member state '(:STOPPED :DEBBUGGER))
                     "CONT" "STOP")))
    (kill-process pid signal)))

(defcommand swap-frames () ()
  (let* ((group (current-group))
         (frames (group-frames group)))
    (assert (eq 2 (length frames)))
    (destructuring-bind (a b) frames
      (let ((awin (frame-window a))
            (bwin (frame-window b)))
        (assert (and awin bwin))
        (let ((win (current-window))
              (win2 (progn (fnext) (current-window))))
          (pull-window win)
          (fnext)
          (pull-window win2))))))

(defcommand center-pointer () ()
  "Move the pointer to the center of the current head."
  (let* ((head (current-head))
         (x (/ (head-width head) 2))
         (y (/ (head-height head) 3))
         (offset 10)
         (delay-secs .1))
    (loop for (dx dy) in '((-1 -1) (1 -1) (-1 1) (1 1) (0 0))
       do
         (warp-pointer (current-screen)
                       (+ x (* dx offset))
                       (+ y (* dy offset)))
       do (sleep delay-secs))))

(defcommand ignore-lid-close-temporarily (hours)
    ((:number "duration in hours: "))
  "Temporarily do nothing if laptop lid is closed."
  (let ((timespec (format nil "now + ~D hours" hours)))
    (run-command-async-notify
     "ignore-lid-close-temporarily.sh"
     (list timespec))))

(defcommand insert-key-with-delay (key)
    ((:string "enter xdotool key name: "))
  "Insert a key by name."
  (sleep .4)
  (message "~A" key)
  (run-shell-command (format nil "xdotool key ~A" key)))

(defcommand paste-primary () ()
  "Paste the primary selection."
  (let ((text (get-x-selection nil '(:PRIMARY))))
    (sleep .4)
    (run-shell-command (format nil "xdotool type ~A" text))))

(defcommand double-click-and-ctrl-c () ()
  "Double click and ctrl+c"
  (run-shell-command "xdotool click 1 click 1 key ctrl+c"))
