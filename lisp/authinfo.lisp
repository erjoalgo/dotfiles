(defpackage :authinfo
  (:use :cl)
  (:export
   #:get-by
   #:get-by-or-error
   #:get-by-machine
   #:alist-get
   #:alist-get-or-error
   #:bind-values
   #:parse
   #:persist-authinfo-line))
(in-package :authinfo)

(defun file-string (path)
  (with-open-file (stream path)
    (let* ((n-estimate (file-length stream))
	  (data (make-string n-estimate))
	  (n (read-sequence data stream)))
      (unless (= n n-estimate)
	(setf data (subseq data 0 n)))
      data)))

(defun parse (&key (filename "~/.authinfo"))
  (when (probe-file filename)
    (loop with contents = (file-string filename)
          for line in (ppcre:split #\Newline contents)
          collect
          (loop for (key val . rest)
                  on (ppcre:split " +" line :omit-unmatched-p t) by #'cddr
                collect (cons (intern (string-upcase key) :keyword) val)))))


(defmacro alist-get (key alist)
  `(cdr (assoc ,key ,alist)))

(defun alist-get-or-error (key alist)
  (or (alist-get key alist)
      (error (format nil "no value for ~A in alist with keys: ~A" key
                     ;; don't show passwords in error message
                     (mapcar #'car alist)))))


(defun get-by (authinfo-key value)
  (loop for alist in (parse)
     as key-value = (alist-get authinfo-key alist)
     thereis (when (equal key-value value) alist)))

(defun get-by-or-error (authinfo-key value)
  (or (get-by authinfo-key value) (error "no entry found with ~A = ~A"
                                         authinfo-key value)))

(defun get-by-machine (host)
  (get-by :machine host))

(defmacro bind-values (entry authinfo-vars &body body)
  (let ((entry-sym (gensym "entry-")))
    `(let*
         ((,entry-sym ,entry)
          ,@(loop for var in authinfo-vars
                 as sym = (intern (symbol-name var) :keyword)
                collect (list var `(alist-get-or-error ,sym ,entry-sym))))
       ,@body)))

(defstruct authinfo-key
  name
  optional
  value
  prompt)

(defun persist-authinfo-line
    (key-specs
     &key
       (authinfo-filename
        (make-pathname :name ".authinfo"
                       :defaults (user-homedir-pathname))))
  (with-open-file (fh authinfo-filename
                      :if-does-not-exist :create
                      :if-exists :append
                      :direction :output)
    (loop with strings
          for key in key-specs
          unless (authinfo-key-p key)
            do (setf key (apply #'make-authinfo-key key))
          do (with-slots (name optional prompt value) key
               (unless value
                 (setf value
                       (or
                        (stumpwm:read-one-line (stumpwm:current-screen)
                                               (or prompt (format nil "enter ~A: " name)))
                        (if optional "" (error "required value not provided: ~A" name)))))
               (push (format nil "~A ~A" name value) strings))
          finally (format fh "~%~{~A~^ ~}" (reverse strings))))
  (car (last (parse))))
