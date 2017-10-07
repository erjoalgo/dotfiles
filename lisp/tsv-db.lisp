(defpackage #:tsv-db
  (:export
   #:make-persistent-alist
   #:persistent-alist-load
   #:persistent-alist-alist
   #:persistent-alist-get
   #:persistent-alist-fn
   #:completing-read-alist
   #:completing-read-alist-key-value
	)
  ;(:use #:util)
  )

(defstruct persistent-alist alist fn)

(defun tsv-to-alist (dir)
  (loop for filename in (directory (make-pathname :name :wild
						  :defaults dir))
     when (not (directory-pathname-p filename))
     collect (cons (pathname-name filename)
		   (with-open-file (stream filename)
		     (read-line stream)))))

(defun tsv-add-entry (dir key value)
  (with-open-file (out (make-pathname :name key
				      :defaults dir)
		       :if-does-not-exist :create
		       :if-exists :err
		       :direction :output)
    (format out "~A%" value)))

(defun persistent-alist-load (palist)
  (setf (persistent-alist-alist palist)
	(tsv-to-alist (persistent-alist-fn palist))))

(defun persistent-alist-load-if-exists (palist)
  (when (probe-file (persistent-alist-fn palist))
    (persistent-alist-load palist)))

(defun persistent-alist-push (palist key value)
  (echo (format nil "before len: ~A" (length (persistent-alist-alist palist))))
  (push (cons key value) (persistent-alist-alist palist))
  (echo (format nil "after len: ~A" (length (persistent-alist-alist palist))))
  (tsv-add-entry (persistent-alist-fn palist)
		 key value))

(defun persistent-alist-get (palist key)
  (cdr (assoc key
	       (persistent-alist-alist
		palist)
	       :test 'equal)))

(defun completing-read-alist (alist &key (prompt "enter key: "))
  (completing-read
   (current-screen)
   prompt
   ;;TODO coerce to strings
   (mapcar 'car alist)))

(defun completing-read-alist-key-value
    (alist
     &key
       (prompt "enter key: ")
       (space-trim t)
       (disallow-empty t)
       (verbose t))

  (let ((key (completing-read-alist alist :prompt prompt)))
    (when key
      (when space-trim (setf key (trim-spaces key)))
      (if (and disallow-empty (= (length key) 0))
	  (and verbose (echo "key must be nonempty")) nil)
	  (let ((key-value (assoc key alist  :test 'equal)))
	    (if (not key-value)
		(and verbose (message "no such key-value for key: '~a'" key) nil)
		key-value)))))

(defmacro define-stumpwm-type-for-completion-from-alist
    (type-name-sym alist-sym)
  `(define-stumpwm-type ,type-name-sym (input prompt)
     (or (argument-pop input)
	 (completing-read-alist-key-value ,alist-sym :prompt prompt)
	 (throw 'error "Abort."))))

(defmacro define-stumpwm-type-for-completion-from-alist-key-only
    (type-name-sym alist-sym)
  `(define-stumpwm-type ,type-name-sym (input prompt)
     (or (argument-pop input)
	 (completing-read-alist ,alist-sym :prompt prompt)
	 (throw 'error "Abort."))))

(defvar *persistent-alist-syms* nil )

;;from LIST, not alist
(defmacro define-stumpwm-type-for-completion-from-list
    (type-name-sym list-sym)
  `(define-stumpwm-type ,type-name-sym (input prompt)
     (or (argument-pop input)
	 (completing-read
	   (current-screen) prompt
	   (mapcar 'symbol-name ,list-sym)))))

(define-stumpwm-type-for-completion-from-list
    :persistent-alist-sym *persistent-alist-syms*)

(defcommand reload-persistent-alist (alist-sym)
    ((:persistent-alist-sym "enter p-alist symbol: " ))
  "reload a persistent alist from its underlying file, message how many entries loaded"
  (let ((p-alist (symbol-value (intern alist-sym "STUMPWM"))))
    (persistent-alist-load p-alist)
    (message "loaded ~D symbols" (length (persistent-alist-alist p-alist)))))


(define-stumpwm-type :symbol (input prompt)
  (or (find-symbol (string-upcase
		    (or (argument-pop input)
			;; Whitespace messes up find-symbol.
			(string-trim " "
				     (or (completing-read (current-screen)
							  prompt
							  ;; find all symbols in the
							  ;;  stumpwm package.
							  (let (acc)
							    (do-symbols (s (find-package "STUMPWM"))
							      (push (string-downcase (symbol-name s)) acc))
							    acc))
					 (throw 'error "Abort.")))
			))
		   "STUMPWM")
      (throw 'error "Symbol not in STUMPWM package")))
