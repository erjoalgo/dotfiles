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

(defcommand sip-call-contact (contact) ((:contact "Enter contact to call: "))
  (assert contact)
  (or
   (when-let* ((phones (contact-phones contact))
               (number (selcand-select phones "select among multiple phone number: "
                                       #'identity t)))
     (sip-call number)
     t)
   (error "no phone number found for contact ~A." contact)))

(defcommand sip-call-terminate () ()
  (linphonecsh "generic" "terminate"))

(defcommand sip-init () ()
  (linphonecsh "init" "-c" (namestring (truename #P"~/.linphonerc"))))

(defcommand sip-exit () ()
  (linphonecsh "exit"))
