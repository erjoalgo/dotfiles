(defpackage :authinfo
  (:use :cl)
  (:export
   #:get-by
   #:get-by-machine
   #:alist-get
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

(defun get-by (authinfo-key value)
  (loop for alist in (parse)
     as key-value = (alist-get authinfo-key alist)
     thereis (when (equal key-value value) alist)))

(defun get-by-machine (host)
  (get-by :machine host))
