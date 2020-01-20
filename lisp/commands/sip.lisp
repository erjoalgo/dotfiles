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

(defcommand sip-call-clipboard () ()
  (let* ((clipboard (get-x-selection nil :clipboard)))
    (sip-call clipboard)))

(defcommand sip-call-prompt (number) ((:string "Enter number: "))
  (when number
    (sip-call number)))

(defcommand sip-call-terminate () ()
  (linphonecsh "generic" "terminate"))

(defcommand sip-init () ()
  (linphonecsh "init" "-c" (namestring (truename #P"~/.linphonerc"))))

(defcommand sip-exit () ()
  (linphonecsh "exit"))
