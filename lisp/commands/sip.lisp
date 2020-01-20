(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (message "running: linphonecsh ~{~A~^ ~}" args)
  (run-command-async "linphonecsh" args nil t))

(defun sip-call (number &key (sip-host "sanjose2.voip.ms"))
  (assert number)
  (let* ((number-clean (ppcre:regex-replace-all "[^0-9]" number ""))
         (intl-prefix-opt "")
         (call-arg (format nil "sip:~A~A@~A"
                           intl-prefix-opt number-clean sip-host)))
    (echo (format nil "calling ~A" call-arg))
    (linphonecsh "dial" call-arg)))

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
