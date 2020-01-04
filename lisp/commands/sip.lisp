(defun sip-call (number &key (sip-host "sanjose2.voip.ms"))
  (assert number)
  (let* ((number-clean (ppcre:regex-replace-all "[^0-9]" number ""))
         (intl-prefix-opt (if (eq (length number-clean) 10) "1" ""))
         (call-arg (format nil "sip:~A~A@~A"
                           intl-prefix-opt number-clean sip-host)))
    (echo (format nil "calling ~A" number))
    (run-command-async "linphone" `("-c" ,call-arg) nil t)))

(defcommand sip-call-clipboard () ()
  (let* ((clipboard (get-x-selection nil :clipboard)))
    (sip-call clipboard)))

(defcommand sip-call-prompt (number) ((:string "Enter number: "))
  (when number
    (sip-call number)))
