(ql:quickload :websocket-driver-client)

(defparameter *sms-fanout-address*
  "wss://sms.erjoalgo.com/fanout?api-key=REDACTED")

(defvar *sms-fanout-client* nil)

(defparameter *sms-fanount-reconnect-interval-mins* 1)

(defun sms-fanout-connected-p (&key (client *sms-fanout-client*))
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

(defun sms-fanout-connect ()
  (when-let ((current (sms-fanout-connected-p)))
    (wsd:close-connection current))
  (let ((client (wsd:make-client *sms-fanout-address*)))
    (wsd:start-connection client)
    (wsd:on :message client
            (lambda (message)
              (let* ((json-data (cl-json:decode-json-from-string message)))
                (format t "~&Got: ~A~%" message)
                (alist-let json-data (to from message status code)
                  (if status
                      (progn (assert (and status code))
                             (format t "ws status: ~A code ~A" status code))
                      (progn
                        (assert (and to from message))
                        '(x-message (format nil "sms-fanout message received: ~A" message))))))))
    (wsd:on :close client
            (lambda (&key code reason)
              (format t "sms-fanout: channel closed: ~A ~A" code reason)
              (setf client nil)))
    (wsd:on :error client
            (lambda (err)
              (message "sms-fanout: channel error ~A ~A" err)
              (setf client nil)))
    ;; (wsd:send client "Hi")
    client))

;;

(def-thread *sms-fanout-reconnect-thread*
    (loop do
         (progn
           (format t "on sms-fanout reconnect loop")
           (unless (sms-fanout-connected-p)
             '(message "sms-fanout reconnect loop: attempting to reconnect")
             (handler-case
                 (setf *sms-fanout-client* (sms-fanout-connect))
               ((or USOCKET:NS-TRY-AGAIN-CONDITION error) (err)
                 (format t "failed to connect: ~A. " err)))
             '(message "sms-fanout reconnect loop: post connect attempt"))
           (when (sms-fanout-connected-p)
             (format t "pinging")
             (wsd:send-ping *sms-fanout-client*))
           (format t "sleeping...")
           (sleep (* *sms-fanount-reconnect-interval-mins* 60)))))

;; (sms-fanout-connect)
;; (sb-thread:terminate-thread *sms-fanout-reconnect-thread*)
