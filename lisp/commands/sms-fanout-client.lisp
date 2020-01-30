(defpackage :sms-fanout-client
  (:use :cl)
  (:export
   #:connect
   #:connected-p
   #:reconnect-loop))
(in-package :sms-fanout-client)

;; (ql:quickload :websocket-driver-client)
;; (ql:quickload :cl-json)

(defvar *sms-fanout-client* nil)

(defun connected-p (&key (client *sms-fanout-client*))
  (and client
       (eq :OPEN (wsd:ready-state client))
       client))

(defun x-message (message)
  (sb-ext:run-program "xmessage" (list message "-center")
                      :search t
                      :wait nil))

(defmacro alist-let (alist vars &rest body)
  (let ((alist-sym (gensym "alist-")))
    `(let ((,alist-sym ,alist))
       (let ,(loop for var in vars
                collect `(,var (cdr (assoc
                                     ,(intern (symbol-name var) :keyword)
                                     ,alist-sym))))
         ,@body))))

(defvar *messages-received* (make-hash-table))

(defun on-message-received (from to message id)
  (unless (gethash id *messages-received*)
    (setf (gethash id *messages-received*) t)
    (let ((text (format nil "sms from ~A (to ~A): ~A" from to message)))
      (stumpwm:message-wrapped text))))

(defun connect (address)
  (stumpwm::when-let ((current (connected-p)))
    (wsd:close-connection current))
  (let ((client (wsd:make-client address)))
    (wsd:start-connection client)
    (wsd:on :message client
            (lambda (message)
              (format t "~&Got: ~A~%" message)
              (let* ((json-data (cl-json:decode-json-from-string message)))
                (alist-let json-data (to from message status code id)
                  (if status
                      (progn (assert (and status code))
                             (format t "ws status: ~A code ~A" status code))
                      (progn
                        (assert (and to from message id))
                        (on-message-received from to message id)))))))
    (wsd:on :close client
            (lambda (&key code reason)
              (format t "sms-fanout: channel closed: ~A ~A" code reason)
              (setf client nil)))
    (wsd:on :error client
            (lambda (err)
              (stumpwm:message "sms-fanout: channel error ~A ~A" err)
              (setf client nil)))
    client))

(defun reconnect-loop (address &key (reconnect-delay-mins 1))
  (loop do
       (progn
         (format t "on sms-fanout reconnect loop")
         (unless (connected-p :client *sms-fanout-client*)
           (format t "sms-fanout reconnect loop: attempting to reconnect")
           (handler-case
               (setf *sms-fanout-client* (connect address))
             ((or USOCKET:NS-TRY-AGAIN-CONDITION error) (err)
               (format t "failed to connect: ~A. " err)))
           (format t "sms-fanout reconnect loop: post connect attempt"))
         (when (connected-p)
           (format t "pinging")
           (wsd:send-ping *sms-fanout-client*))
         (format t "sleeping...")
         (sleep (* reconnect-delay-mins 60)))))

;; (sms-fanout-connect)
;; (sb-thread:terminate-thread *sms-fanout-reconnect-thread*)
