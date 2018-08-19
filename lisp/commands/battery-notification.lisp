
(defvar *battery-notification-thread* nil)

(when (and *battery-notification-thread*
           (sb-thread:thread-alive-p *battery-notification-thread*))
  (sb-thread:terminate-thread *battery-notification-thread*))

(setf *battery-notification-thread*
      (sb-thread:make-thread 'battery-info-check-notify-loop))
