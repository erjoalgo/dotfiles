(defpackage cladaver
  (:use :cl))

(in-package #:cladaver)

(defstruct server-info
  base-url username password)

(defmacro if-let-ok (on-error-spec bindings &rest on-success)
  (if (null bindings)
      `(progn ,@on-success)
      (destructuring-bind ((val-var val-or-err) . rest) bindings
        `(handle-error
          (signal-to-error ,val-or-err)
          (,val-var (if-let-ok ,on-error-spec ,rest ,@on-success))
          ,on-error-spec))))

(defmacro make-error (error-message)
  `(values nil ,error-message))

(defmacro handle-error (form
                        &optional
                          on-success-spec
                          on-error-spec)
  (destructuring-bind
        ((val-var on-success) (err-var on-error))
      `(
        ,(or on-success-spec
             (let ((sym (gensym "val-var-"))) `(,sym ,sym)))
        ,(or on-error-spec
             (let ((sym (gensym "err-var-"))) `(,sym (make-error ,sym)))))
    `(multiple-value-bind (,val-var ,err-var) ,form
       (if (null ,err-var) ,on-success
           ,on-error))))

(defmacro error-to-signal (form)
  `(handle-error ,form
                 nil
                 (err
                  (error "error on ~A: ~A" ',form err))))

(defmacro signal-to-error (form)
  `(handler-case ,form
     (error (err-signal)
       (make-error err-signal))))

(defmacro ->? (forms &optional on-error-spec)
  (let ((val-var (gensym "val-var-")))
    (if (null (cadr forms))
        `(handle-error ,(car forms)
                       (,val-var ,val-var)
                       ,on-error-spec)
        (destructuring-bind (first second . rest) forms
          `(handle-error
            (signal-to-error (,(car second) ,first ,@(cdr second)))
            (,val-var (->? ,val-var ,@rest))
            ,on-error-spec)))))

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
  (with-slots (base-url) info
    (if-let-ok nil
     ((url (format nil "~A~A" base-url path))
      (raw-resp
       (http-request-or-error url
                              :method :PROPFIND
                              :additional-headers '(("Depth" . "1"))))
      (doc (cxml:parse raw-resp
                       (stp:make-builder)))
      (nodeset
       (xpath:with-namespaces (("D" "DAV:"))
         (xpath:evaluate "//D:href/text()" doc)))
      (iter (xpath:make-node-set-iterator nodeset)))
     (loop while (not (xpath:node-set-iterator-end-p iter))
        as node = (xpath:node-set-iterator-current iter)
        as data = (cxml-stp:data node)
        collect data
        do (setf iter (xpath:node-set-iterator-next iter))))))

(defun cat (info path)
  (with-slots (base-url) info
    (if-let-ok nil
        ((url (format nil "~A~A" base-url path))
         (raw-resp
          (http-request-or-error url
                                 :method :GET)))
      raw-resp)))

(defun put (info path data)
  (with-slots (base-url) info
    (if-let-ok nil
        ((url (format nil "~A~A" base-url path))
         (raw-resp
          (http-request-or-error url
                                 :method :PUT
                                 :content data)))
      raw-resp)))

'(let* ((info (make-server-info :base-url "https://my-webdav-server.com"))
       (urls (error-to-signal (ls info "/urls"))))
  (error-to-signal (cat info "/urls/a.txt"))
  (error-to-signal (put info "/urls/a.txt" "hola")))
