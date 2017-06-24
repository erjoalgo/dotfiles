(defpackage #:mozrepl)

(ql:quickload "usocket")

(defvar *nc-timeout-millis* 3000)
(defvar *nc-default-buff-size* 3000)
(defun nc (host port data &key wait)
  "write some data to a tcp socket"
  (let* ((socket (usocket:socket-connect host port))
	 (stream (usocket:socket-stream socket))
	 seq)
    (princ data stream)
    (force-output stream)
    (when wait
      (setf seq (make-sequence 'string *nc-default-buff-size*))
      (usocket:wait-for-input socket :timeout 1 :ready-only t)
      (loop
	 with start-time-millis = (get-internal-real-time)
	 with timeout = (if (numberp wait) wait *nc-timeout-millis*)
	 with i = 0
	 as char = (read-char-no-hang stream)
	 as time-elapsed = (- (get-internal-real-time) start-time-millis)
	 as time-left = (- timeout time-elapsed)
	 while (or char (> time-left 0)) do
	   (when char (setf (aref seq i) char) (incf i))
	 finally (setf seq (subseq seq 0 i)))
      (usocket:socket-close socket))
    (unless wait
      (sb-thread:make-thread
       (lambda ()
	 (sleep (/ *nc-timeout-millis* 1000))
	 (usocket:socket-close socket))))
    seq))

(defvar *mozrepl-port* 4242)
(defvar *localhost* "127.0.0.1")

(defun mozrepl-send-command (cmd &key wait)
  ;;for now starting a new process for each cmd. better to keep a single pipe open
  ;;but risk corrupting state of the repl with malformed input
  (let* ((out
	  (nc *localhost* *mozrepl-port* cmd :wait wait)))
    (or
     (not wait)
     (ppcre::register-groups-bind
      (resp)
      ((format nil "repl[0-9]*> (.*)~%repl[0-9]*>") out)
      resp)

     (progn
       (echo-string-list
	(current-screen)
	(list "error with mozrepl input:"
	      cmd
	      out)
	(setf STUMPWM::ex (cons cmd out)))
       (print out t)))))

;;no one uses this
(defun mozrepl-send-commands-delay (cmds &key (delay .1))
  (loop for cmd in cmds
	do (sleep delay)
     do (mozrepl-send-command cmd)))

(defun mozrepl-firefox-get-url ()
  (let ((out (mozrepl-send-command "content.document.location.href"
				   :wait t)))
    (subseq-minus out 1 -1)))

(defun escape-dqs (string)
  (ppcre:regex-replace-all "\""
			   string
			   "\\\""))

(defun mozrepl-firefox-new-tab (url)
  ;;taken from:
  ;;https://gist.github.com/jabbalaci/a1312d211c110ff3855d
  ;;https://developer.mozilla.org/en-US/Add-ons/Code_snippets/Tabbed_browser
  (mozrepl-send-command
   (format nil
	   "gBrowser.selectedTab = gBrowser.addTab(\"~A\");"
	   (escape-dqs url))))

;;please never again...
'(defun get-firefox-url-clipboard ()
  (sleep .5)
  ;;(run-shell-command "xdotool key --delay 50 Ctrl+l Ctrl+c" t)
  (send-meta-key (current-screen) (kbd "C-l"))
  (send-meta-key (current-screen) (kbd "C-c"))
  ;;(sleep .5)
  (sleep .5)
  (get-x-selection ))
