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

(defcommand move-window-toggle ()
    () "move window to next monitor (display)"
  (move-window
   (if (neighbour :right
                  (tile-group-current-frame (current-group))
		  (group-frames (current-group)))
       :right
       :left)))

(defcommand dict-lookup-command (word)
    ((:string "Word to lookup: "))
  "lookup a word in the dictionary (requires the dict package)"
  (let* (
	 (definition
	   (run-shell-command (format nil "dict ~a" word) t))
	 (*suppress-echo-timeout* t); let me read the definition in peace
	 )
    (when word
      (log-timestamped-entry word *vocab-fn*)
      (message "definition: ~a" definition))))

(defcommand echo-pointer ()     ()
  "echo the mouse location"
  (echo (run-shell-command "xdotool getmouselocation" t)))

(defcommand magnus () ()
  (run-shell-command "magnus"))

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

(defcommand screen-lock-interactive () ()
  (screen-lock "interactive"))

(defcommand screen-lock (caller-name) ()
  (message-wrapped "screen-lock called by ~A" caller-name)
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

(defcommand pulseaudio-load-module-loopback () ()
  "Double click and ctrl+c"
  (run-shell-command "pactl load-module module-loopback")
  (run-shell-command "pactl load-module module-echo-cancel"))

(defcommand pulseaudio-unload-module-loopback () ()
  "Double click and ctrl+c"
  (run-shell-command "pactl unload-module module-loopback"))

(defcommand toggle-screen-key () ()
  "Toggle the screenkey utility"
  (run-shell-command
   "bash -c 'if pgrep screenkey; then pkill screenkey; else screenkey; fi '"))

(defun beep-fn (&key
                  (freq 880)
                  (duration-secs .25))
  (run-shell-command
   (format nil
           "timeout -s9 ~F speaker-test -t sine -f ~D -l 1"
           duration-secs
           freq)))

(defcommand beep () ()
  "Fake a pcspkr beep using alsa speaker-test.

   Avoids issues with beep permissions"
  (beep-fn))

(defun get-lat-lng ()
  "TODO geolocation API"
  (cons 28.562871 -81.210339))

(defun redshift-oneshot (cmd)
  "Run a 'oneshot' redshift command"
  (destructuring-bind (lat . lng) (get-lat-lng)
    (let ((shell-cmd (format nil
                             "redshift -l~A:~A -ov ~A"
                             lat lng cmd)))
      (format t "~A" shell-cmd)
      (run-shell-command shell-cmd))))

(defcommand redshift-shift-red () ()
  "Redshift shift red"
  (redshift-shift -500))

(defcommand redshift-shift-blue () ()
  "Redshift shift red"
  (redshift-shift 500))

(defvar *redshift-temp* 5000)
(defconstant +redshift-min-temp+ 1000)
(defconstant +redshift-max-temp+ 25000)

(defun redshift-shift (delta)
  "Redshift shift red"
  (let* ((curr *redshift-temp*)
         (temp (+ delta curr))
         (temp (max temp +redshift-min-temp+))
         (temp (min temp +redshift-max-temp+)))
    ;; (redshift-oneshot (format nil "-Pt ~A:~A" temp temp))
    (redshift-stop-service)
    (redshift-oneshot (format nil "-PO ~A" temp))
    (setf *redshift-temp* temp)))

(defcommand redshift-reset () ()
  "Redshift shift red"
  (redshift-oneshot "-x"))

(defun redshift-current-temp ()
  (let ((output
          (run-shell-command
           "redshift -p | grep -Po '(?<=Color temperature: )[0-9]+'"
           t)))
    (parse-integer output)))

(defun redshift-stop-service ()
  (run-shell-command "systemctl --user stop redshift.service"))

(defcommand describe-key-+ (keys) ((:key-seq "Describe key:"))
  (with-message-queuing t
    (describe-key keys)
    (message "keys are: ~A" keys)))

(define-key *help-map* (kbd "k") "describe-key-+")

(defcommand unmap-all-message-windows-command () ()
  (unmap-all-message-windows))

(defun password-id-from-url (url)
  (let* ((host (multiple-value-bind (scheme user host)
                   (quri:parse-uri url)
                 host))
         (components (ppcre:split "[.]" host))
         (password-id (second (reverse components))))
    password-id))

(defun ledger-read-password-id ()
  (let* ((url (mozrepl:chrome-get-url))
         (password-id
           (read-one-line (current-screen) "enter ledger password id: "
                          :initial-input (if url (password-id-from-url url) ""))))
    password-id))

(defcommand ledger-password-backup-restore
    ;; (password-id) ((:string ))
    () ()
  (let* ((password-id (ledger-read-password-id)))
    (run-shell-command (format nil "ledger-password-backup-restore.js ~A" password-id))))

(defun type-sh (&key password-id  no-retry)
  (let* ((password-id (or password-id (ledger-read-password-id)))
         (on-error-callback
           (unless no-retry
             `(lambda () (type-sh ,password-id :no-retry t)))))
    (assert password-id)
    (run-command-async-notify
     "type.sh"
     (list password-id)
     nil
     on-error-callback)))

(defcommand type-sh-command () ()
  (type-sh))

(defcommand kinit () ()
  "kinit admin && aklog -d"
  (let ((pass (read-one-line (current-screen) "enter kinit password: " :password t))
        proc
        output)
    (with-input-from-string (in-fh pass)
      (setf output
            (with-output-to-string (out-fh)
              (setf proc
                    (sb-ext:run-program "kinit" '("admin")
                                        :input in-fh
                                        :output out-fh
                                        :error out-fh
                                        :search t
                                        :wait t)))))
    (unless (zerop (slot-value proc 'SB-IMPL::%EXIT-CODE))
      (error "kinit failed: ~A" output))

    (let (proc
          output)
      (setf output
            (with-output-to-string (out-fh)
              (setf proc
                    (sb-ext:run-program "aklog" '("-d")
                                        :output out-fh
                                        :error out-fh
                                        :search t
                                        :wait t))))
      (unless (zerop (slot-value proc 'SB-IMPL::%EXIT-CODE))
        (error "aklog failed: ~A" output)))
    (message "^2kinit && aklog -d succeeded")))

(defun adb-select-device () ()
  (let* ((device-line
           (selcand:select
            :candidates (->> (run-shell-command "adb devices" t)
                             (ppcre:split #\Newline)
                             (cdr))
            :prompt "select adb device: "
            :display-candidates t
            :on-empty-error "no adb devices found!"))
         (device-id (car (ppcre:split #\Tab device-line))))
    device-id))

(defun openafs-this-cell ()
  (format nil "~A/" (trim-spaces (file-string #P"/etc/openafs/ThisCell"))))

(defun openafs-this-cell-root ()
  (merge-pathnames (openafs-this-cell) "/afs/"))

(defun symlinkp (pathname)
  (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat pathname))))

(defvar last-rom-symlink #P"~/.n64-last-rom")

(defun n64-list-roms ()
  (let* ((dirnames
           (list
            (merge-pathnames #P"public/n64/roms/*.*" (openafs-this-cell-root))
            #P"~/Downloads/n64/*.*"))
         (roms (loop for dirname in dirnames
                     nconc (directory dirname))))
    roms))

(defun n64-select-rom ()
  (let* ((candidates (n64-list-roms))
         (last-rom-truepath
           (when (probe-file last-rom-symlink)
             (truename last-rom-symlink)))
         initial-candidate
         selection)
    (when last-rom-truepath
      (setf candidates
            (cons last-rom-truepath
                  (delete last-rom-truepath candidates :test #'equal)))
      (setf initial-candidate 0))
    (setf selection
          (selcand:select
           :candidates candidates
           :prompt "select n64 rom to play: "
           :display-candidates t
           :stringify-fn #'pathname-name
           ;; :initial-candidate-index initial-candidate
           :on-empty-error (format nil "no n64 roms found ")))
    (when (or (probe-file last-rom-symlink)
              (SYMLINKP last-rom-symlink))
      (sb-posix:unlink last-rom-symlink))
    (sb-posix:symlink selection last-rom-symlink)
    selection))


(defcommand press-ir-button (button-name) ((:string))
  (message "pressing button: ~A" button-name)
  (run-command-async-notify-on-error
   "ir-remote.py"
   (list "-b" button-name)))


(defcommand stop-window () ()
  (let ((pid (window-pid (current-window))))
    (kill-process pid "SIGSTOP")))

(defcommand cont-window () ()
  (let ((pid (window-pid (current-window))))
    (assert pid)
    (kill-process pid "SIGCONT")))

(defun pgrep (pattern)
  (let ((lines
          (->
           (format nil "pgrep ~A" pattern)
           (run-shell-command t)
           trim-spaces)))
    (->> (ppcre:split #\Newline lines)
         (mapcar #'parse-integer))))

(defun pkill (pattern)
  (run-shell-command (format nil "pkill ~A" pattern) t))

(defcommand chrome-restart () ()
  (start-porcess-with-logging "chrome-restart.sh"))

(defcommand garage-door-toggle () ()
  (let* ((hostname "garage-door.arpa"))
    (start-porcess-with-logging "shelly-relay.sh"
                                (list "-n" hostname "-s" "on"))))
