(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (message "running: linphonecsh ~{~A~^ ~}" args)
  (run-command-async "linphonecsh" args nil t))

(defvar *sip-default-host* "sanjose2.voip.ms")

(defun sip-phone-number-to-address (number &key (sip-host *sip-default-host*))
  (let* ((number-clean (ppcre:regex-replace-all "[^0-9]" number ""))
         (intl-prefix-opt "")
         (sip-address (format nil "sip:~A~A@~A"
                              intl-prefix-opt number-clean sip-host)))
    sip-address))

(defun sip-call (number)
  (assert number)
  (linphonecsh "dial" (sip-phone-number-to-address number)))

(defcommand sip-call-selection () ()
  (let* ((clipboard (get-x-selection nil :clipboard)))
    (sip-call clipboard)))

(defcommand sip-call-number (number) ((:string "Enter number: "))
  (assert (stringp number))
  (sip-call number))

(defcommand sip-call-contact (contact-number)
    ((:contact-number "Enter contact to call: "))
  (assert contact-number)
  (sip-call contact-number))

(defcommand sip-call-terminate () ()
  (linphonecsh "generic" "terminate"))

(defun sip-sms-send (sip-address message)
  ;; TODO add "chat" command to linphonecsh
  (linphonecsh "generic"
               (format nil "chat ~A '~A'"
                       sip-address message)))

(defcommand sip-sms-send-selection (message)
    ((:string "Enter SMS message: "))
  (let* ((clipboard (get-x-selection nil :clipboard)))
    (sip-sms-send clipboard message)))

(defcommand sip-sms-send-number (number message)
    ((:string "Enter number: ") (:string "Enter SMS message: "))
  (assert (stringp number))
  (sip-sms-send number message))

(defcommand sip-sms-send-contact (contact-number message)
    ((:contact-number "Enter contact to sms-send: ")
     (:string "Enter SMS message: "))
  (assert contact-number)
  (sip-sms-send contact-number message))

(defcommand sip-init () ()
  (linphonecsh "init" "-c" (namestring (truename #P"~/.linphonerc"))))

(defcommand sip-exit () ()
  (linphonecsh "exit"))
