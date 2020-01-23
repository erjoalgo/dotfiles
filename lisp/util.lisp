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

(defun log-entry-timestamped (entry fn)
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

'(defun which (program-name)
  (loop
    with program-pathname = (pathname program-name)
    for path-directory in (ppcre:split #\: (getenv "PATH"))
      thereis
      (loop for pathname in (directory
                             (make-pathname :name :WILD
                                            :type :WILD
                                            :defaults (pathname-as-directory path-directory)))
              thereis (and
                       (pathname-name (probe-file pathname)) ;; regular file
                       (PATHNAME-IS-EXECUTABLE-P pathname)   ;; executable
                       (equal (pathname-name pathname) (pathname-name program-pathname)) ;;name matches
                       (equal (pathname-type pathname) (pathname-type program-pathname)) ;;extension matches, if any
                       pathname))))

(defun which (program-name)
  (let* ((cmd (format nil "which ~A" program-name))
         (out (run-shell-command cmd t))
         (trimmed (trim-spaces out)))
    (unless (zerop (length trimmed)) trimmed)))

(defun window-pid (win)
  (car (xlib:get-property (WINDOW-XWIN win) :_NET_WM_PID)))

(defmacro eval-async (&body form)
  `(sb-thread:make-thread
    (lambda () ,@form)))

(defun run-command-retcode-output (command &optional args)
  (let (proc string)
    (setf string (with-output-to-string (out)
                   (setf proc
                         (sb-ext:run-program
                          command args
                          :wait t
                          :output out
                          :search t))))
    (values (sb-ext:process-exit-code proc)
            string)))

(defun run-command-sync-notify-on-error (command args)
  (multiple-value-bind (retcode output)
      (run-command-retcode-output command args)
    (unless (zerop retcode)
      (message-wrapped "error: ~A ~A failed with ~A: ~A"
                       command args (zerop retcode) output))))

;; TODO optional
(defmacro run-command-async (command
                             &optional
                               args
                               retcode-output-vars
                               on-success on-error)
  (destructuring-bind (retcode-sym output-sym) (or retcode-output-vars '(nil nil))
    (setf retcode-sym (or retcode-sym (gensym "retcode-"))
          output-sym (or output-sym (gensym "output-")))
    `(eval-async
       (multiple-value-bind (,retcode-sym ,output-sym)
           (run-command-retcode-output ,command ,args)
         (if (zerop ,retcode-sym)
             ,(or on-success
                  `(message-wrapped "^2success of '~A ~{~A~^ ~}'^*"
                                    ;; TODO eval command only once
                                    ,command ,args))
             ,(or on-error
                  `(message-wrapped
                    "^1non-zero exit: ~A of '~A ~{~A~^ ~}': ~A^*"
                    ,retcode-sym
                    ,command ,args
                    ,output-sym)))))))

(defun run-command-async-notify (command &optional args)
  (run-command-async command (mapcar 'princ-to-string args) (ret out)
                     (message-wrapped "^2success of '~A ~{~A~^ ~}'^*"
                                      command args)
                     (message-wrapped
                      "^1non-zero exit: ~~A of '~A ~{~A~^ ~}': ~~A^*"
                      command args
                      ret out)))

(defun message-wrapped (fmt &rest args)
  (let* ((text (apply #'format nil fmt args))
         (screen (current-screen))
         (pixels-per-char (text-line-width
                          (screen-font screen) "a"
                          :translate #'translate-id))
         (pixels-per-line (screen-width screen))
         (chars-per-line (floor pixels-per-line pixels-per-char)))
    (assert (> chars-per-line 0))
    (loop with len = (length text)
          with idx = 0
          while (< idx len)
          as new-idx = (+ idx chars-per-line)
          collect (subseq text idx (min len new-idx)) into chunks
          do (setf idx new-idx)
          finally (echo-string-list screen chunks))))
