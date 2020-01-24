;; TODO clos
(in-package :STUMPWM)

(defstruct psym-driver
  list-serialized-records ;; pathname => '(serialized1, serialized2 ...)
  deserialize-record   ;; serialized => record
  serialize-record   ;; (pathname, record) => serialized
  )

(defstruct psym
  records
  pathnames
  driver
  short-description
  )

(defun psym-concrete-pathnames (psym &key include-nonexistent)
  (loop for pathname-possibly-wild in (psym-pathnames psym)
     append (if (WILD-PATHNAME-P pathname-possibly-wild)
                (directory pathname-possibly-wild)
                (when (or include-nonexistent (probe-file pathname-possibly-wild))
                  (list pathname-possibly-wild)))))

(defmacro wrap-safe (form err-sym error-form)
  `(handler-case
       ,form
     (error (,err-sym) ,error-form)))

(defun psym-load (psym &key (verbose t))
  (loop for pathname in (psym-concrete-pathnames psym)
        with list-serialized = (psym-driver-list-serialized-records (psym-driver psym))
        with deserialize-fun = (psym-driver-deserialize-record (psym-driver psym))
        as serialized = (wrap-safe
                         (funcall list-serialized pathname)
                         err (warn "error listing ~A: ~A" pathname err))
        append (wrap-safe (mapcar deserialize-fun serialized) err
                          (warn "error loading records from ~A (~A): ~A"
                                serialized pathname err))
          into records
        finally
           (progn
             (when verbose
               (message "loaded ~D ~A" (length records)
                        (or (psym-short-description psym) "records")))
             (setf (psym-records psym) records))))

(defun psym-add (psym record &optional pathname)
  (unless pathname
    (setf pathname (or (car (psym-concrete-pathnames psym))
                       (error "no concrete pathnames for psym ~A" psym))))
  (let ((serialize-fun (psym-driver-serialize-record (psym-driver psym))))
    (funcall serialize-fun pathname record)
    (push record (psym-records psym))))

(defun file-to-lines (pathname)
  (ppcre:split #\Newline (file-string pathname)))

(defparameter psym-tsv-alist-driver
  (let ((separator "	"))
    (declare (ignore separator))
    (make-psym-driver
     :list-serialized-records #'file-to-lines
     :deserialize-record (lambda (line)
                           ;; tab used below
                           (or
                            (ppcre:register-groups-bind (key val) ("^([^	]+)	(.*)$" line)
                              (cons key val))
                            (error "non-tsv line")))
     ;; tab used below
     :serialize-record (lambda (pathname record)
                         (destructuring-bind (key . value) record
                           (with-open-file (fh pathname
			                       :if-does-not-exist :create
			                       :if-exists :append
			                       :direction :output)
                             (format fh "~A	~A~%" key value))))))
  "a psym driver for tab-separated files")

(defparameter psym-dir-alist-driver
  (make-psym-driver
   :list-serialized-records (lambda (pathname-top)
                              (remove-if-not
                               (lambda (pathname) (pathname-name (probe-file pathname)))
                               (directory
                                (make-pathname :name :WILD
                                               :defaults
                                               (uiop:ensure-directory-pathname pathname-top)))))
   :deserialize-record (lambda (pathname-record)
                         (cons (pathname-name pathname-record) (file-string pathname-record)))

   :serialize-record (lambda (pathname-top record)
                       (destructuring-bind (key . value) record
                         (with-open-file (fh (make-pathname :name key
                                                            :defaults pathname-top)
			                     :if-does-not-exist :create
			                     :if-exists :SUPERSEDE
			                     :direction :output)
                           (format fh "~A" value))))))

(defparameter psym-lines-list-driver
  (make-psym-driver
   :list-serialized-records #'file-to-lines
   :deserialize-record #'identity
   :serialize-record (lambda (pathname-top record)
                       (format t "serializing to ~A..." pathname-top)
                       (with-open-file (fh pathname-top
			                   :if-does-not-exist :create
			                   :if-exists :append
			                   :direction :output)
                         (format fh "~A~%" record)))))

(defmacro define-stumpwm-type-with-completion
    (type-name (list-sym list-form)
     &key
       (require-match t)
       (disallow-empty t)
       (space-trim t)
       (default-prompt (format nil "select ~A: " list-form))
       ((:sel-form (sel form))
        (let ((sym (gensym "selection")))
          (list sym sym))))

  `(define-stumpwm-type ,type-name (input prompt)
     (or (argument-pop input)
         (let* ((,list-sym ,list-form)
                (,sel (completing-read (current-screen)
                                       (or prompt ,default-prompt)
                                       ,list-sym
                                       :require-match ,require-match)))
           (when ,sel
             (when ,space-trim (setf ,sel (trim-spaces ,sel)))
             (if (and ,disallow-empty (= (length ,sel) 0))
                 (error "sel must be nonempty")
                 ,form)))
	 (throw 'error "Abort."))))

(defun alist-get (key alist)
  (let ((key-value (assoc key alist  :test 'equal)))
    (if (not key-value)
	(error "no such key: '~a'" key)
	key-value)))

(defun common-prefix (strings)
  (loop with prefix = (car strings)
        for string in (cdr strings)
        as ln = (loop for c across prefix
                      for cc across string
                      for i from 0
                      while (eq c cc)
                      finally (return i))
        when (not (= ln (length prefix)))
          do (setf prefix (subseq prefix 0 ln))
        finally (return prefix)))

(defun select-pathname (pathnames &key (prompt "select pathname: ")
                                    (require-match t))
  (let* ((namestrings (mapcar #'namestring pathnames))
         (prefix (common-prefix namestrings))
         (sel (completing-read (current-screen) prompt
                               (mapcar (lambda (namestring) (subseq namestring (length prefix)))
                                       namestrings)
                               :require-match require-match))
         (sel-namestring (concat prefix sel)))
    (pathname sel-namestring)))

(defmacro define-stumpwm-type-pathname (type-name pathnames-form)
  `(define-stumpwm-type ,type-name (input prompt)
     (or (argument-pop input)
         (select-pathname ,pathnames-form :prompt prompt)
         (throw 'error "Abort."))))
