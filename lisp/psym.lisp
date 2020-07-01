;; TODO clos
(in-package :STUMPWM)

(defclass psym ()
  ((records
    :initarg :records
    :accessor psym-records)
   (pathnames
    :initarg :pathnames
    :accessor psym-pathnames)
   (short-description
    :initarg :short-description
    :accessor psym-short-description)))

;; list-serialized-records ;; pathname => '(serialized1, serialized2 ...)
;; deserialize-record   ;; serialized => record
;; serialize-record   ;; (pathname, record) => serialized

(defmethod psym-non-wild-pathnames ((psym psym) &key include-nonexistent)
  (loop for pathname in (psym-pathnames psym)
        if (WILD-PATHNAME-P pathname)
          append (directory pathname)
        else when
             (or include-nonexistent
                 (probe-file pathname))
        collect pathname))

(defmacro wrap-safe (form err-sym error-form)
  `(handler-case
       ,form
     (error (,err-sym) ,error-form)))

(defmethod psym-load ((psym psym) &key (verbose t))
  (loop for pathname in (psym-non-wild-pathnames psym)
        as serialized = (psym-list-serialized-records psym pathname)
        append (mapcar
                (lambda (serialized-record)
                  (psym-deserialize-record psym serialized-record))
                serialized)
          into records
        finally
           (progn
             (when verbose
               (message "loaded ~D ~A" (length records)
                        (or (psym-short-description psym) "records")))
             (setf (psym-records psym) records))))

(defmethod psym-add ((psym psym) record &optional pathname)
  (unless pathname
    (setf pathname (or (car (psym-non-wild-pathnames psym))
                       (error "no regular pathnames for psym ~A" psym))))
  (psym-serialize-record psym pathname record)
  (push record (psym-records psym)))

(defun file-to-lines (pathname)
  (ppcre:split #\Newline (file-string pathname)))

(defclass psym-tsv (psym) ()
  ;; a psym subclass for tab-separated files
  )

(defmethod psym-list-serialized-records ((psym psym-tsv) pathname)
  (file-to-lines pathname))

(defmethod psym-deserialize-record ((psym psym-tsv) line)
  (let ((regexp (format nil "^([^~C]+)~C(.*)$" #\tab #\tab)))
    ;; TODO(ejalfonso) compile regexp
    (or
     (ppcre:register-groups-bind (key val) (regexp line)
       (cons key val))
     (error "non-tsv line"))))


(defmethod psym-serialize-record ((psym psym-tsv) pathname record)
  (destructuring-bind (key . value) record
    (with-open-file (fh pathname
                        :if-does-not-exist :create
                        :if-exists :append
                        :direction :output)
      (format fh "~A~C~A~%" key #\tab value))))

(defclass psym-dir-alist (psym) ()
  ;; a psym subclass for records as files in a directory
  )

(defmethod psym-list-serialized-records ((psym psym-dir-alist) pathname-top)
  (remove-if-not
   (lambda (pathname) (pathname-name (probe-file pathname)))
   (directory
    (make-pathname :name :WILD
                   :defaults
                   (uiop:ensure-directory-pathname pathname-top)))))

(defmethod psym-deserialize-record ((psym psym-dir-alist) pathname-record)
  (cons (pathname-name pathname-record)
        (file-string pathname-record)))


(defmethod psym-serialize-record ((psym psym-dir-alist) pathname record)
  (destructuring-bind (key . value) record
    (with-open-file (fh (make-pathname :name key
                                       :defaults pathname)
                        :if-does-not-exist :create
                        :if-exists :SUPERSEDE
                        :direction :output)
      (format fh "~A" value))))

(defclass psym-lines-list (psym) ()
  ;; a psym subclass for records as a set of lines in a file
  )

(defmethod psym-list-serialized-records ((psym psym-lines-list) pathname)
  (file-to-lines pathname))

(defmethod psym-deserialize-record ((psym psym-lines-list) record)
  record)

(defmethod psym-serialize-record ((psym psym-lines-list) pathname record)
  (with-open-file (fh pathname
                      :if-does-not-exist :create
                      :if-exists :append
                      :direction :output)
    (format fh "~A~%" record)))

(defclass psym-webdav-alist (psym) ()
  ;; a psym subclass for records as files in a directory
  )

(defmethod psym-list-serialized-records ((psym psym-dir-alist) pathname-top)
  (remove-if-not
   (lambda (pathname) (pathname-name (probe-file pathname)))
   (directory
    (make-pathname :name :WILD
                   :defaults
                   (uiop:ensure-directory-pathname pathname-top)))))

(defmethod psym-deserialize-record ((psym psym-dir-alist) pathname-record)
  (cons (pathname-name pathname-record)
        (file-string pathname-record)))


(defmethod psym-serialize-record ((psym psym-dir-alist) pathname record)
  (destructuring-bind (key . value) record
    (with-open-file (fh (make-pathname :name key
                                       :defaults pathname)
                        :if-does-not-exist :create
                        :if-exists :SUPERSEDE
                        :direction :output)
      (format fh "~A" value))))

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
