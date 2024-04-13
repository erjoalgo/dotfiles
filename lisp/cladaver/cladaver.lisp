(defpackage cladaver
  (:use :cl :statusor)
  (:export
   #:make-server-info
   #:ls
   #:cat
   #:put
   #:mkdir))

(in-package #:cladaver)

(defstruct server-info
  base-url username password)

(defmacro http-request-or-error (url &rest args)
  `(multiple-value-bind (body status-code)
       (drakma:http-request ,url ,@args)
     (if (= (floor status-code 100) 2)
         ;; 2xx
         body
         (make-error
          (format nil "non-2xx status code: ~A on url ~A: ~A"
                  status-code ,url body )))))

(defun ls (info path)
  (with-slots (base-url username password) info
    (if-let-ok nil
               ((url (format nil "~A~A" base-url path))
                (output (curl url
                              :method "PROPFIND"
                              :username username
                              :password password
                              :headers `(("Depth" . "1"))))
                (doc (cxml:parse output (stp:make-builder)))
                (nodeset
                 (xpath:with-namespaces (("D" "DAV:"))
                   (xpath:evaluate "//D:href/text()" doc)))
                (iter (xpath:make-node-set-iterator nodeset)))
               (loop
                 with first = nil
                 while (not (xpath:node-set-iterator-end-p iter))
                 for i from 0
                 as node = (xpath:node-set-iterator-current iter)
                 as pathname = (pathname (cxml-stp:data node))
                 as basename = (pathname-name pathname)
                 do (assert (if (zerop i)
                                (prog1
                                    (null basename)
                                  (setf first pathname))
                                (equal (pathname-directory first)
                                       (pathname-directory pathname))))
                 unless (null basename)
                   collect pathname
                 do (setf iter (xpath:node-set-iterator-next iter))))))

(defun curl (url &key (method "GET") username password data headers)
  (let (output error proc args)
    (push (format nil "-X~A" method) args)
    (push (format nil "-u~A:~A" username password) args)
    (push url args)
    (when data
      (setf args (append args `("-d" ,data))))
    (loop for (k . v) in headers
          do (push (format nil "~A:~A" k v) args))
    (setf output
          (with-output-to-string (fh-out)
            (setf error
                  (with-output-to-string (fh-err)
                    (setf proc
                          (sb-ext:run-program
                           "curl"
                           args
                           :search t :output fh-out
                           :error fh-err))))))
    (let ((exit-code (slot-value proc 'SB-IMPL::%EXIT-CODE)))
      (if (zerop exit-code) output
          (error (format nil "non-zero exit status ~A for curl ~A: ~A"
                         exit-code args error))))))


(defun cat (info path)
  (with-slots (base-url username password) info
    (if-let-ok nil
               ((url (format nil "~A~A" base-url path))
                (output (curl url :method "GET" :username username :password password)))
               output)))

(defun put (info path data)
  (with-slots (base-url username password) info
    (if-let-ok nil
               ((url (format nil "~A~A" base-url path))
                (output
                 (curl url :method "GET" :username username :password password :data data)))
               output)))

(defun mkdir (info path)
  (with-slots (base-url username password) info
    (if-let-ok nil
               ((url (format nil "~A~A" base-url path))
                (output
                 (curl url :method "MKCOL" :username username :password password)))
               output)))
