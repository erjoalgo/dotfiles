(defpackage :authinfo
  (:use :cl)
  (:export
   #:get-by
   #:get-by-or-error
   #:get-by-machine
   #:alist-get
   #:alist-get-or-error
   #:get-info-assert-vars
   #:parse
   #:persist-authinfo-line))
(in-package :authinfo)

(defun parse (&key (filename "~/.authinfo"))
  (when (probe-file filename)
    (loop with contents = (stumpwm:file-string filename)
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

(defmacro get-info-assert-vars (authinfo-key value authinfo-vars &body body)
  (let ((alist-sym (gensym "alist-")))
    `(let*
         ((,alist-sym (or (get-by-or-error ,authinfo-key ,value)))
          ,@(loop for var in authinfo-vars
                 as sym = (intern (symbol-name var) :keyword)
                collect (list var `(alist-get-or-error ,sym ,alist-sym))))
       ,@body)))

(defun persist-authinfo-line (
                              &key
                              required-keys
                              optional-keys
                              line-prefix
                              ;; (noecho-keys '(:password))
                              (authinfo-filename
                               (make-pathname :name ".authinfo"
                                              :defaults (user-homedir-pathname))))
  (loop with strings
        for key in (append required-keys optional-keys)
        as name = (if (symbolp key)
                      (string-downcase (symbol-name key))
                      key)
        ;; TODO check if (find key-sym noecho-keys)
        as value = (stumpwm:read-one-line
                    (stumpwm:current-screen)
                    (format nil "enter ~A (~A): " name line-prefix))
        if value
          do (push (format nil "~A ~A" name value) strings)
        else if (member key required-keys :test #'equal)
               do (error "required key not provided: ~A" key)
        finally
           (when line-prefix (push line-prefix strings))
           (with-open-file (fh authinfo-filename
                               :if-does-not-exist :create
                               :if-exists :append
                               :direction :output)
             (format fh "~%~{~A~^ ~}~%" strings))
        (parse)))
