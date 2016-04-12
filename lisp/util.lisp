;;this file should only contain defuns

(defun file-string (path)
  (with-open-file (stream path)
    (let* ((n-estimate (file-length stream))
	  (data (make-string n-estimate))
	  (n (read-sequence data stream)))
      (unless (= n n-estimate)
	(setf data (subseq data 0 n)))
      data)))

;thanks to some contributor from #stumpwm irc. scottj
;http://dpaste.com/132AZ0G
(defun deep-copy-map (source-map)
  (let ((new-map (make-sparse-keymap)))
           (dolist (binding (kmap-bindings source-map))
             (let ((key (binding-key binding))
                   (command (binding-command binding)))
               (define-key new-map key command)))
           new-map))

(defun curry (fun &rest args)
    (lambda (&rest new-args)
      (apply fun  (append args new-args))))
(defun compose (f g)
  (let* ((f f)
	 (g g))
    ;(lambda (&rest args) (apply f (apply g args)))))
    (lambda (&rest args) (funcall f (apply g args)))))

(defun compose-apply (f g)
  (let* ((f f)
	 (g g))
    (lambda (&rest args) (apply f (apply g args)))))
(defmacro fset  (name fun)
  `(defun ,name (&rest args) (apply ,fun args)))


(defun reverse-fun-args (fun)
  (compose (curry 'apply fun) (compose 'reverse 'list))
  ;(compose (curry 'apply fun) 'list)
  )



(defun my-echo ()
  (reverse (screen-last-msg (current-screen))))

(defun hash-from-list (l)
  (let* ((hash (make-hash-table :test 'equal)))
    (mapcar (lambda (el) (setf (gethash el hash) t))
	    l)
    hash))

(defun list-from-hash (h &optional fun)
  (let* (
	 (l nil )
	 (fun (or fun (lambda (&rest els) els)))
	 )
    (maphash (lambda (k v) (setq l (cons (funcall fun k v) l) ))
	     h)
    
    l)
  )

(defmacro toggle-var (var)
  `(setf ,var (not ,var)))




(defun escape-bash-single-quotes (text)
  (ppcre:regex-replace-all "[']" text "'\\\\''"))


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

(defun extract-match (regexp string i)
  (multiple-value-bind (m res)
      (ppcre::scan-to-strings regexp string)
    (declare (ignore m))
    ;res)
    (and res (aref res (1- i)))) 
  )

;;this doesn't short-circuit


(defun join (joiner &rest strings)
  (if (cdr strings)
      (reduce (lambda (cum new) (concat cum joiner new))
	      strings)
      (car strings)))

(defun subseq-minus (seq start &optional end)
  "allow negative indices"  
  (let* ((len (length seq))
	 (minus-convert (lambda (x) (if (< x 0) (+ len x) x)))
	 (start (funcall minus-convert start))
	 (end (funcall minus-convert end)))
    (subseq seq start end)))

;;silly
(fset string-trim-whitespace (curry 'string-trim
				    '(#\Space #\Newline #\Backspace #\Tab 
				      #\Linefeed #\Page #\Return #\Rubout)))

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

;;directly from define-stumpwm-type example
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
