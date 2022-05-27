(in-package :STUMPWM)

(defun battery-info ()
  (let* ((devices (run-shell-command "upower -e" t))
         (device (loop for device in (cl-ppcre:split #\Newline devices)
                         thereis (and (cl-ppcre:scan "BAT|battery" device)
                                      device)))
         (info (run-shell-command (format nil "upower -i ~A" device) t)))
    ;; example:
    ;; ((NATIVE-PATH . "BAT0")
    ;;   (VENDOR . "SMP")
    ;;   (MODEL . "DELL GPM0365")
    ;;   (SERIAL . "376")
    ;;   (|POWER SUPPLY| . "yes")
    ;;   (|HAS HISTORY| . "yes")
    ;;   (|HAS STATISTICS| . "yes")
    ;;   (PRESENT . "yes")
    ;;   (RECHARGEABLE . "yes")
    ;;   (STATE . "charging")
    ;;   (WARNING-LEVEL . "none")
    ;;   (ENERGY . "17.8866 Wh")
    ;;   (ENERGY-EMPTY . "0 Wh")
    ;;   (ENERGY-FULL . "93.2748 Wh")
    ;;   (ENERGY-FULL-DESIGN . "97.0026 Wh")
    ;;   (ENERGY-RATE . "47.937 W")
    ;;   (VOLTAGE . "11.952 V")
    ;;   (|TIME TO FULL| . "1.6 hours")
    ;;   (PERCENTAGE . "19%")
    ;;   (CAPACITY . "96.157%")
    ;;   (TECHNOLOGY . "lithium-ion")
    ;;   (ICON-NAME . "'battery-low-charging-symbolic'"))
    (loop for line in (cl-ppcre:split #\Newline info)
          as kv = (cl-ppcre:split #\: line)
          when (= 2 (length kv))
            collect (cons (intern (string-upcase (trim-spaces (car kv))) :keyword)
                          (trim-spaces (cadr kv))))))

(defun battery-notification-maybe-notify (&key (thresholds
                                                '(20 10 5 4 2)))
  "Maybe notify that battery is low. Thresholds has the form (see source): "
  (destructuring-bind
      (min-percent-message
       min-percent-flashing-message
       min-percent-window-popup
       min-percent-beep
       min-percent-suspend)
      thresholds
    (erjoalgo-webutil:with-json-paths (battery-info)
        (state (percentage-string "percentage"))
      (statusor:if-let-ok
       (err (error "battery notification error: ~A" err))
       ((_ (when (or (null percentage-string) (null state))
             (statusor:make-error "unable to parse percentage or state")))
        (percentage (prog1
                        ;; ignore multiple values,
                        ;; which are interpreted as a status error
                        (parse-integer percentage-string :junk-allowed t))))
       (format t "battery-notification t7es: value of percentage: ~A~%" percentage)
       (when (equal state "discharging")
         (format t "trace: battery-notification dft0~%")
         (when (<= percentage min-percent-message)
           (format t "trace: battery-notification 9zuv~%")
           (let ((*message-window-gravity* :top-right))
             (message "^1 warning: battery discharging (~D%)^*" percentage)))
         (when (<= percentage min-percent-flashing-message)
           (format t "trace: battery-notification cx3p~%")
           (loop for i below (- min-percent-message percentage)
                 do
                    (let ((*message-window-gravity* :top-right))
                      (message
                       "^1 warning: battery discharging (~D%)^*" percentage)
                      (sleep .5)
                      (unmap-all-message-windows)
                      (sleep .5))))
         (when (<= percentage min-percent-window-popup)
           (run-shell-command "xmessage battery low!"))
         (when (<= percentage min-percent-beep)
           (beep))
         (when (<= percentage min-percent-suspend)
           (run-shell-command "systemctl suspend")))))))

(defun battery-info-check-notify-loop (&key (interval-secs 30))
  (loop
    do
       (battery-notification-maybe-notify)
    do (sleep (* interval-secs))))
