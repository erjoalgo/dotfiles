(defun file-string (path)
  (with-open-file (stream path)
    (let* ((n-estimate (file-length stream))
	  (data (make-string n-estimate))
	  (n (read-sequence data stream)))
      (unless (= n n-estimate)
	(setf data (subseq data 0 n)))
      data)))

(defun deep-copy-map (source-map)
  (let ((new-map (make-sparse-keymap)))
    (dolist (binding (kmap-bindings source-map))
      (let ((key (binding-key binding))
	    (command (binding-command binding)))
	(define-key new-map key command)))
    new-map))

;;this fixes a certain issue with Virtualbox and stumpwm
(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (cond
    ;; ignore asynchronous window errors
    ((and asynchronous
          (find error-key '(xlib:window-error xlib:drawable-error xlib:match-error)))
     (dformat 4 "Ignoring error: ~s~%" error-key))
    ((eq error-key 'xlib:access-error)
     (write-line "Another window manager is running.")
     (write-line (prin1-to-string error-key) )
     (write-line (prin1-to-string key-vals))
     ;(and (boundp 'ab) (write-line (prin1-to-string ab)))

     ;(throw :top-level :quit)
     )
     ;; all other asynchronous errors are printed.
     (asynchronous
      (message "Caught Asynchronous X Error: ~s ~s" error-key key-vals))
     (t
      (apply 'error error-key :display display :error-key error-key key-vals))))

(defun subseq-minus (seq start &optional end)
  "allow negative indices"
  (let* ((len (length seq))
	 (start (mod start len))
	 (end (and end (mod end len))))
    (subseq seq start end)))

(defvar *whitespace-char-list* '(#\Space #\Newline #\Backspace #\Tab
				      #\Linefeed #\Page #\Return #\Rubout))
(defun trim-spaces (str)
  (string-trim '(#\space #\tab #\newline) str))


(defun log-entry-timestamp (entry fn)
  (with-open-file (fh fn
		      :if-does-not-exist :create
		      :if-exists :append
		      :direction :output
		      )
		  (format fh "~A~A~A~%"
			  entry
			  (coerce '(#\Tab) 'string)
			  (time-date-and-time))))

;;reference. directly from define-stumpwm-type example
'(define-stumpwm-type :symbol (input prompt)
  (or (find-symbol (string-upcase
		    (or (argument-pop input)
			;; Whitespace messes up find-symbol.
			(string-trim \" \"
				     (completing-read (current-screen)
						      prompt
						      ;; find all symbols in the
						      ;;  stumpwm package.
						      (let (acc)
							(do-symbols (s (find-package \"STUMPWM\"))
							  (push (string-downcase (symbol-name s)) acc))
							acc)))
			(throw 'error \"Abort.\")))
		   \"STUMPWM\")
      (throw 'error \"Symbol not in STUMPWM package\")))

(defmacro define-stumpwm-type-for-completion
    (sym completion-form)
  `(define-stumpwm-type ,sym (input prompt)
     (or (argument-pop input)
	 (completing-read
	  (current-screen)
	  prompt
	  ,completion-form))))

(defun hide-message-windows ()
  (when (timer-p *message-window-timer*)
            (cancel-timer *message-window-timer*)
            (setf *message-window-timer* nil))
    (progn (unmap-all-message-windows) (unmap-all-message-windows)))

(defun last-msgs ()
  (screen-last-msg (current-screen)))

(defun expand-user (fn)
  (cl-ppcre:regex-replace "^~/" fn (user-homedir-pathname)))

(defmacro -> (&rest forms)
  (if (cadr forms)
      ;;(destructuring-bind (first (a a-rest) . rest) forms
      ;;`(-> a first a-rest ,@rest))
      (destructuring-bind (first second . rest) forms
	(destructuring-bind (a . a-rest) (if (atom second)
					     (cons second nil)
					   second)
	  `(-> ,(apply 'list a first a-rest) ,@rest)))
    (car forms)))

(defmacro ->> (&rest forms)
  (if (second forms)
      (destructuring-bind (a b . cde) forms
	(let ((b (if (atom b) (list b) b)))
	  `(->> ,(nconc b (list a)) ,@cde)))
      (first forms)))

(defmacro define-stumpwm-type-from-wild-pathname
    (type-name-sym wild-pathanme &key allow-nonexistent)
  `(define-stumpwm-type ,type-name-sym (input prompt)
     (or (argument-pop input)
         (let ((selection (completing-read (current-screen)
                                           prompt
                                           (mapcar #'pathname-name
                                                   (directory ,wild-pathanme))
                                           :require-match ,(not allow-nonexistent))))
           (when selection
             (merge-pathnames selection ,wild-pathanme)))
	 (throw 'error "Abort."))))
