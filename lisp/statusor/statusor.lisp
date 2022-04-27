(defpackage statusor
  (:use :cl)
  (:export
   #:if-let-ok
   #:make-error
   #:handle-error
   #:error-to-signal
   #:signal-to-error
   #:nil-to-error
   #:->?
   #:return-if-error))

(in-package #:statusor)

(defmacro if-let-ok (on-error-spec bindings &rest on-success)
  (if (null bindings)
      `(progn ,@on-success)
      (destructuring-bind ((var-ok val-or-err) . rest) bindings
        `(handle-error
          (signal-to-error ,val-or-err)
          (,var-ok (if-let-ok ,on-error-spec ,rest ,@on-success))
          ,on-error-spec))))

(defmacro make-error (error-message)
  `(values nil ,error-message))

(defmacro handle-error (form
                        &optional
                          on-success-spec
                          on-error-spec)
  (destructuring-bind
        ((var on-success) (err-var on-error))
      `(
        ,(or on-success-spec
             (let ((sym (gensym "var-"))) `(,sym ,sym)))
        ,(or on-error-spec
             (let ((sym (gensym "err-var-"))) `(,sym (make-error ,sym)))))
    `(multiple-value-bind (,var ,err-var) ,form
       ,@(when (equal "_" (symbol-name var))
           `((declare (ignore ,var))))
       (if (null ,err-var) ,on-success
           ,on-error))))

(defmacro error-to-signal (form)
  `(handle-error ,form
                 nil
                 (err
                  (error "error on ~A:~% ~A" ',form err))))

(defmacro signal-to-error (form &optional error-message-prefix)
  (let ((error-signal-sym (gensym "error-signal-")))
    `(handler-case ,form
       (error (,error-signal-sym)
         (make-error
          ,(if error-message-prefix
               `(format nil "~A: ~A" ,error-message-prefix ,error-signal-sym)
               error-signal-sym))))))

(defmacro nil-to-error (form &optional error-message)
  `(or ,form
       (make-error
        ,(or error-message (format nil "~A is nil" form)))))

(defmacro ->? (forms &optional on-error-spec)
  (let ((var (gensym "var-")))
    (if (null (cadr forms))
        `(handle-error ,(car forms)
                       (,var ,var)
                       ,on-error-spec)
        (destructuring-bind (first second . rest) forms
          `(handle-error
            (signal-to-error (,(car second) ,first ,@(cdr second)))
            (,var (->? ,var ,@rest))
            ,on-error-spec)))))

(defmacro return-if-error (form &optional block-name)
  `(handle-error ,form (success-val success-val)
                 (err ,(if block-name
                           `(return-from ,block-name (make-error err))
                           `(return (make-error err))))))
