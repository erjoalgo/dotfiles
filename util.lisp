;;this file should only contain defuns

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

;thanks to some contributor from #stumpwm irc. scottj
;http://dpaste.com/132AZ0G
(defun deep-copy-map (source-map)
  (let ((new-map (make-sparse-keymap)))
           (dolist (binding (kmap-bindings source-map))
             (let ((key (binding-key binding))
                   (command (binding-command binding)))
               (define-key new-map key command)))
           new-map)
  )

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

(defmacro stumpwm::toggle-var (var)
  `(setq ,var (not (and (boundp ',var) ,var)))
  )


(defun expand-user (fn)
  ;TODO replace with home directory
  (cl-ppcre:regex-replace "^~/" fn HOME )
  )

(defun escape-bash-single-quotes (text)
  (ppcre:regex-replace-all "[']" text "'\\\\''")
  )


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
     (and (boundp 'ab) (write-line (prin1-to-string ab)))
     
     ;(throw :top-level :quit)
     )
     ;; all other asynchronous errors are printed.
     (asynchronous
      (message "Caught Asynchronous X Error: ~s ~s" error-key key-vals))
     (t
      (apply 'error error-key :display display :error-key error-key key-vals))))

(defun extract-match (regexp string i)
  (multiple-value-bind (m res) (ppcre::scan-to-strings regexp string)
    ;res)
    (and res (aref res (1- i)))) 
  )

;;this doesn't short-circuit
(defun contains (elm L)
  (reduce (lambda (cum el) (or cum (equal el elm))) L :initial-value nil ))

(defun contains (elm L)
  (and L (or (equal (car L) elm) (contains elm (cdr L)))))

(defun any (func L)
  (and L (or (funcall func (car L)) (any func (cdr L))))
  )

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
