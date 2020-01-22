
(defparameter *sms-fanout-address*
  "wss://sms.erjoalgo.com")

(defparameter *sms-fanount-reconnect-interval-mins* 1)

(defun sms-fanout-connected-p (&key (client *sms-fanout-client*))
  (and client
       (eq :OPEN (wsd:ready-state client))))

(defun sms-fanout-read-loop ()
  (message "starting sms-fanout-read-loop thread.")
  (loop
     with proc = (sb-ext:run-program "ws" (list *sms-fanout-address*)
                                     :search t
                                     :input :STREAM
                                     :output :STREAM
                                     ;; :output t
                                     :error :output)
     with stream = (sb-ext:process-output proc)
     as line = (read-line stream nil :eof)
     do (message-wrapped "sms-fanout: ~A" line)
     while (not (eq line :eof)))
  (message "ended sms-fanout-read-loop thread."))

(def-thread *sms-fanout-loop-thread*
  (sms-fanout-read-loop))

(defun sms-fanout-stop ()
  (when *sms-fanout-loop-thread*
    (sb-thread:terminate-thread *sms-fanout-reconnect-thread*)))
;; (sms-fanout-stop)
