(defpackage :authinfo
  (:use :cl)
  (:export
   #:get-by
   #:get-by-or-error
   #:get-by-machine
   #:alist-get
   #:alist-get-or-error
   #:get-info-assert-vars
   #:parse))
(in-package :authinfo)

(defun parse (&key (filename "~/.authinfo"))
  (assert (probe-file filename) (filename)
          "authinfo doesn't exist: ~A" filename)
  (loop with contents = (stumpwm:file-string filename)
     for line in (ppcre:split #\Newline contents)
     collect
       (loop for (key val . rest)
          on (ppcre:split #\Space line) by #'cddr
          collect (cons (intern (string-upcase key) :keyword) val))))


(defun alist-get (key alist)
  (cdr (assoc key alist)))

(defun alist-get-or-error (key alist)
  (or (cdr (assoc key alist))
      (error "no value for ~A in ~A" key alist)))

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
