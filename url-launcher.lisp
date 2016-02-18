(setq *launcher-data-fn* (stumpwm-merger "sensitive/url_launcher_data"))

(defun file-has-changed (fn)
  t )
  ;;t)

(defcommand update-launcher-alist () ()
  (setq *launcher-alist* (read-launcher-alist))
  )

(defun maybe-update-launcher-alist ()
  (if (file-has-changed *launcher-data-fn*)
      (update-launcher-alist)
      ))


(setq *url-command-rules* '(
    ("(^https?://.*|.*[.]html.*).*" . "firefox")
    (".*[.]pdf" . "zathura")
    ;;(".*[.]pdf" . "evince")
    ;(".*[.]pdf" . "gv")
    (".*[.](docx?|odt)" . "libreoffice")
    ("about:config" . "firefox")
    ))

(defun url-command (url)
  (let ((url url))
    (any (lambda (rule)
	   (and (cl-ppcre:scan (car rule) url) (cdr rule)))
	 *url-command-rules*)
    )
  )

(defcommand launch-url () ()
  (maybe-update-launcher-alist)
  (let* ((key (completing-read  (current-screen)
				;;"enter url key: " (mapcar 'car *launcher-alist*) :require-match t ))
				"enter url key: " (mapcar 'car *launcher-alist*) ))
	 (line (and key (assoc key *launcher-alist*  :test 'equal)))
	 (url nil )
	 (cmd nil )
	 )
    (echo (format nil  "key is ~A~%" key))
    (when key
      (setq key (trim-spaces key))
      (echo (format nil "actual key was: '~a'" key))
      (if (or (not key) (equal "" key))
	  (echo "key must be nonempty")
	  (if (not line)
	      (progn 
		(print *launcher-alist*)
		(message "no such value for key: '~a'" key)
		)
	      (progn 
					;(setq url (cdr line))
		(setq url (expand-user (cadr line)))
		
		(setq cmd (format nil "~a '~a'&" (url-command url) url))
		(print cmd)
		(run-shell-command cmd)
		)
	      )
	  )
      )
    )
  )

(defun get-firefox-url-clipboard ()
  (sleep .5)
  ;;(run-shell-command "xdotool key --delay 50 Ctrl+l Ctrl+c" t)
  (send-meta-key (current-screen) (kbd "C-l"))
  (send-meta-key (current-screen) (kbd "C-c"))
  ;;(sleep .5)
  (sleep .5)
  (get-x-selection )
  )

(defun get-firefox-url-mozrepl ()
  (let* ((out (run-shell-command "echo 'content.document.location.href' | nc localhost 4242 -q 1" t)))
    (ppcre::register-groups-bind (nil url) ("repl([0-9]+)?> \"([^\"]+)" out)
      url))
  )

(defun trim-spaces (str)
  (string-trim '(#\space #\tab #\newline) str)
  )

(defcommand launcher-append-url (key &optional url)
    (
     (:string "enter new key: ")
     (:string nil )
     ;;((get-firefox-url))
     ;;(:string "enter url: ")
     )
  (setq url (or url (get-firefox-url-mozrepl))
	key (trim-spaces key)
	)
  (if (not (and key url (> (length key) 0) (> (length url) 0)))
      (echo (format nil 
	     "something failed: (and key url (> (length key) 0) (> (length url) 0)) ~a ~a ~a ~a"
	     (not (not key))
	     (not (not url))
	     (> (length key) 0)
	     (> (length url) 0)
	     ))
      (progn 
	(with-open-file (out *launcher-data-fn*
			     :if-does-not-exist :create
			     :if-exists :append
			     :direction :output
			     )
	  (format out "~A"
		  (concat key
			  (coerce '(#\Tab) 'string)
			  (escape-bash-single-quotes url)
			  (coerce '(#\Newline) 'string)))
	  )
	;;(setq *launcher-alist* (cons key url))
	(update-launcher-alist)
	(echo (format nil "added: ~A" url))
	))
  )

(defun read-launcher-alist ()
  (let* (
	 (contents (file-string *launcher-data-fn*))
	 ;;(regex (format nil "[~a~a]" (coerce '(#\Newline) 'string) (coerce '(#\Tab) 'string) ))
	 (regexp '(:sequence
		  :MULTI-LINE-MODE-P
		  :START-ANCHOR
		  (:GREEDY-REPETITION 0 nil (:INVERTED-CHAR-CLASS  #\Tab))
		  #\Tab
		  (:GREEDY-REPETITION 0 nil (:INVERTED-CHAR-CLASS  #\Newline))
		  :END-ANCHOR
		  ))
	 (matches  (ppcre::all-matches-as-strings  regexp (file-string *launcher-data-fn*)))
	 )
    ;;(reverse (loop for el on  by 'cddr
    ;;collect (cons (car el) (cadr el))))
    '(reverse (loop for el in matches
		collect (destructuring-bind (key val) (cl-ppcre:split #\Tab el)
			  (cons key val))
		   ))
    (reverse (loop for el in matches
		collect (cl-ppcre:split #\Tab el)
		  ))
    )
  )


(update-launcher-alist)
