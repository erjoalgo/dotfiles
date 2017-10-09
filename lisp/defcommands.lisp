
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
  (run-shell-command "wifi-connect"))




;;(emacs-in-group)


(defparameter *scrots-top*
  (merge-pathnames  "pictures/auto-scrots/" (user-homedir-pathname)))





(defcommand scrot-cmd (name)
    ((:string "enter name for scrot: "))
  (take-scrot name :fullscreen-p nil))

(defcommand scrot-cmd-full-screen (name)
    ((:string "enter name for scrot: "))
  (take-scrot name :fullscreen-p t))

(defun image-fn-to-text (image-fn)
  (let ((cmd (format nil "tesseract ~A -" image-fn)))
    (run-shell-command cmd t)))


(defcommand ocr-scrot-clipboard () ()
  "interactively prompt for a region of the screen, take a screenshot,
perform ocr on it, place ocr'd text into clipboard"
  (let* ((ocr-name "last-ocr")
	 (ocr-png-filename (take-scrot ocr-name
				       :fullscreen-p nil
				       :verbose nil
				       :eog-scrot nil))
	 (ocr-text (image-fn-to-text ocr-png-filename)))
    (set-x-selection ocr-text :clipboard)
    (message "copied ocr of length ~D to clipboard..."
	     (length ocr-text))))


(defun take-scrot (name &key
			  (fullscreen-p nil)
			  (scrot-top *scrots-top*)
			  (verbose t)
			  (eog-scrot t))
  "save a scrot to *scrots-top*"
  (ensure-directories-exist scrot-top)
  ;;TODO allow selecting region
  (unmap-message-window (current-screen))
  (sleep 1)
  (let* ((out-png-pathname (merge-pathnames
			    (make-pathname :name name :type "png")
			    scrot-top))
	 (out-png (namestring out-png-pathname)))

    (when (cl-ppcre:all-matches "\\s" out-png)
      (error "filename may not contain spaces: ~A" out-png))

    (SB-EXT:RUN-PROGRAM "shutter"
			(append (list "-e" "-f" "-o" out-png "-n")
				(unless fullscreen-p (list "-s")))
			:search t
			:output t
			:error t
			:wait t)
    ;;why does this crash the session
    (when eog-scrot
      (SB-EXT:RUN-PROGRAM "eog"
			  (list out-png)
			  :search t
			  :wait nil))
    (set-x-selection out-png :clipboard)
    (when verbose
      (message "copied to cliboard: ~A" out-png))
    out-png))

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

'(defcommand last-macro
    () ()
  (run-shell-command
   "sleep 1 && xdotool key Tab type 11/06/1990 && xdotool key Tab Return && sleep 3 && xdotool key Tab type 5285 && xdotool key Tab Return"))


(defun gen-xdotool-cmd (&rest specs)
  (format nil "~{~A~^ && ~}"
	  (mapcar (lambda (spec) (format nil "~A ~A"
					 (if (numberp spec) "sleep" "xdotool")
					 spec))
		  specs)))


(defvar *current-prefix-arg* "emacs-like prefix argument")

(defcommand set-prefix-arg (count) ((:number "enter prefix arg count: "))
  (echo (format nil "from ~A to ~A" *current-prefix-arg* count))
  (setf *current-prefix-arg* count)
  )


(defcommand last-macro
    () ()
  (let ((cmds (list 1
		    "key Tab type 11/06/1990"
		    "key Tab Return"
		    3
		    "key Tab type 5285"
		    "key Tab Return")))

    '("click 3"
      .5
      "key w"
      2
      "key Ctrl+s"
      1
      "key Return"
      1
      "key F4")

    ;; (echo (apply 'gen-xdotool-cmd cmds))
    (run-shell-command
     (format nil
	     "for _ in $(seq ~D); do ~A; done"
	     (or *current-prefix-arg* 1)
	     (apply 'gen-xdotool-cmd cmds))))
  '(gen-xdotool-cmd .5
    "click 3"
    .5
    "key h"
    .5
    "key t"
    2
    )
  (setf *current-prefix-arg* nil)
  )



(defcommand type-clipboard-contents () ()
  (let* ((clipboard (get-x-selection)))
    (run-shell-command (format nil "xdotool type ~A" clipboard))))
