(defpackage statusor
  (:use :cl)
  (:export
   #:if-let-ok
   #:make-error
   #:handle-error
   #:error-to-signal
   #:signal-to-error
   #:nil-to-error
   #:->?))

(in-package #:statusor)

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

(defmacro nil-to-error (form &optional error-message)
  `(or ,form
       (make-error
        ,(or error-message (format nil "~A is nil" form)))))

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
