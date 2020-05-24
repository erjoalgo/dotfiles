(in-package :STUMPWM)

(defun file-string (path)
  (with-open-file (stream path)
    (let* ((n-estimate (file-length stream))
	  (data (make-string n-estimate))
	  (n (read-sequence data stream)))
      (unless (= n n-estimate)
	(setf data (subseq data 0 n)))
      data)))

(export '(file-string) :STUMPWM)

(defun machine-get-uuid ()
  (let ((fstab (file-string #P"/etc/fstab")))
    (cl-ppcre:register-groups-bind
        (root-device-name root)
        ("(?m)^([^	 ]+)[ 	]+(/)[	 ]+" fstab)
      (declare (ignore root))
      (assert fstab)
      (let* ((cmd (format nil "lsblk ~A -o uuid" root-device-name))
             (lsblk-out (run-shell-command cmd t))
             (uuid (second (cl-ppcre:split #\Newline lsblk-out))))
        (assert uuid)
        uuid))))

(defun ensure-directory-exists (pathname &key (max-parents 0))
  (unless (probe-file pathname)
    (let ((parent
            (uiop:pathname-parent-directory-pathname
             (uiop:ensure-directory-pathname pathname))))
      (when (and (not (equal parent pathname))
                 (or (eq t max-parents) (> max-parents 0)))
        (ensure-directory-exists
         parent
         :max-parents (if (eq t max-parents) t (1- max-parents)))))
    (SB-POSIX:MKDIR pathname #o775)))

(defun deep-copy-map (source-map)
  (let ((new-map (make-sparse-keymap)))
    (dolist (binding (kmap-bindings source-map))
      (let ((key (binding-key binding))
	    (command (binding-command binding)))
	(define-key new-map key command)))
    new-map))

(defvar *whitespace-chars* '(#\Space #\Newline #\Backspace #\Tab
			     #\Linefeed #\Page #\Return #\Rubout))
(defun trim-spaces (str)
  (string-trim '(#\space #\tab #\newline) str))

(defun string-blank-p (string)
  (zerop (length (trim-spaces string))))

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

(export '(window-pid) :STUMPWM)

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
    (loop for text in (ppcre:split #\Newline text)
       with chunks
         do
    (loop with len = (length text)
          with idx = 0
          while (< idx len)
       as new-idx = (+ idx chars-per-line)
          do (push (subseq text idx (min len new-idx)) chunks)
       do (setf idx new-idx))
       finally (echo-string-list screen (nreverse chunks)))))

(export '(message-wrapped) :STUMPWM)

(defparameter *message-colors*
  '(:black
    :red
    :green
    :yellow
    :blue
    :magenta
    :cyan
    :white)
  "Message colors.")

(defun message-colorize (msg color)
  (let* ((color-sym (-> color princ-to-string string-upcase (intern :keyword)))
         (idx (position color-sym *message-colors*)))
    (if idx
        (format nil "^~D~A^*" idx msg)
        (error "no such color: ~A. choices: ~A"
               color
               *message-colors*))))

(export '(message-colorize *message-colors*) :STUMPWM)

(defmacro def-thread-start (thread-var &body body)
  `(progn
     (defvar ,thread-var nil)
     (when (and ,thread-var
                (sb-thread:thread-alive-p ,thread-var))
       (sb-thread:terminate-thread ,thread-var))
     (setf ,thread-var
           (sb-thread:make-thread (lambda () ,@body)))))

(defun first-existing-file (&rest files)
  (loop for file in files thereis
       (and (probe-file (parse-namestring file))
	    file)))

(defun first-existing-command (&rest commands)
  "assume command has no spaces or funny characters"
  (->> (run-shell-command (format nil "which ~{~A~^ ~}" commands) t)
       (cl-ppcre:split #\Newline)
       (car)))

(defmacro with-elapsed-time (elapsed-time-ms-var form &body body)
  (let ((start-time-sym (gensym "start-time")))
    `(let ((,start-time-sym (get-internal-real-time)))
       ,form
       (let ((,elapsed-time-ms-var (- (get-internal-real-time) ,start-time-sym)))
         ,@body))))

(define-stumpwm-type :non-blank-string (input prompt)
  (or (argument-pop input)
      (let ((line (read-one-line (current-screen) prompt)))
        (when (and line (not (string-blank-p line)))
          line))
      (throw 'error "Abort.")))

(defmacro define-stumpwm-type-with-completion
    (type-name list-form
     &key
       (key-fn 'identity)
       (value-fn 'identity)
       (no-hints t))
  `(define-stumpwm-type ,type-name (input prompt)
     (or
      (argument-pop input)
      (when-let*
          ((selection
            (selcand:select
             :candidates ,list-form
             :prompt prompt
             :stringify-fn (function ,key-fn)
             :autoselect-if-single t
             :no-hints ,no-hints)))
        (,value-fn selection))
      (throw 'error "Abort"))))
