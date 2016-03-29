(defpackage #:mozrepl)

(defun nc (host port data &key wait)
  (with-input-from-string
   (input-fh data)
   (if (not wait)
       (SB-EXT:RUN-PROGRAM "nc" (list host port "-q2")
			   ;;TODO output to tmp?
			   :search t
			   :wait nil 
			   :output t
			   :error t
			   :input input-fh)
     (with-output-to-string
       (output-fh)
       (SB-EXT:RUN-PROGRAM "nc" (list host port "-q2")
			   ;;TODO output to tmp?
			   :search t
			   :wait t
			   :output output-fh
			   :error output-fh
			   :input input-fh)))))

(defvar *mozrepl-port* "4242")

(defun mozrepl-send-command (cmd &key wait)
  ;;for now starting a new process for each cmd. better to keep a single pipe open
  ;;but risk corrupting state of the repl with malformed input
  (let* ((out
	  (nc "localhost" *mozrepl-port* cmd :wait wait)))
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

(defun mozrepl-firefox-new-tab (url)
  ;;taken from:
  ;;https://gist.github.com/jabbalaci/a1312d211c110ff3855d
  ;;https://developer.mozilla.org/en-US/Add-ons/Code_snippets/Tabbed_browser
  (mozrepl-send-command
   (format nil
	   "gBrowser.selectedTab = gBrowser.addTab(\"~A\");"
	   (escape-dqs url))))

(defun escape-dqs (string)
  (ppcre:regex-replace-all "\""
			   string
			   "\\\""))

  


;;please never again...
'(defun get-firefox-url-clipboard ()
  (sleep .5)
  ;;(run-shell-command "xdotool key --delay 50 Ctrl+l Ctrl+c" t)
  (send-meta-key (current-screen) (kbd "C-l"))
  (send-meta-key (current-screen) (kbd "C-c"))
  ;;(sleep .5)
  (sleep .5)
  (get-x-selection ))
