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
         (raw-resp
          (http-request-or-error url
                                 :method :PROPFIND
                                 :additional-headers '(("Depth" . "1"))
                                 :basic-authorization (when (and username password)
                                                        (list username password))))
         (doc (cxml:parse raw-resp (stp:make-builder)))
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

(defun cat (info path)
  (with-slots (base-url username password) info
    (if-let-ok nil
        ((url (format nil "~A~A" base-url path))
         (raw-resp
          (http-request-or-error url :method :GET
                                 :basic-authorization (when (and username password)
                                                        (list username password))))
         (string (if (stringp raw-resp)
                     raw-resp
                     (babel:octets-to-string raw-resp))))
      string)))

(defun put (info path data)
  (with-slots (base-url username password) info
    (if-let-ok nil
        ((url (format nil "~A~A" base-url path))
         (raw-resp
          (http-request-or-error url
                                 :method :PUT
                                 :content data
                                 :basic-authorization (when (and username password)
                                                        (list username password)))))
        raw-resp)))

(defun mkdir (info path)
  (with-slots (base-url username password) info
    (if-let-ok nil
               ((url (format nil "~A~A" base-url path))
                (raw-resp
                 (http-request-or-error
                  url
                  :method :MKCOL
                  :basic-authorization (when (and username password)
                                         (list username password)))))
               raw-resp)))
