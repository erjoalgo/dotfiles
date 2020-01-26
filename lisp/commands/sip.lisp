(defpackage :sip
  (:use :cl)
  (:export
   #:call
   #:linphonecsh
   #:sms-send
   #:phone-number-to-address))
(in-package :sip)

(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (stumpwm:message "running: linphonecsh ~{~A~^ ~}" args)
  (stumpwm::run-command-async "linphonecsh" args nil t))

(defvar *sip-default-host* "sanjose2.voip.ms")

(defun phone-number-to-address (number &key (sip-host *sip-default-host*))
  (let* ((number-clean (ppcre:regex-replace-all "[^0-9]" number ""))
         (intl-prefix-opt "")
         (sip-address (format nil "sip:~A~A@~A"
                              intl-prefix-opt number-clean sip-host)))
    sip-address))

(defun call (number)
  (assert number)
  (linphonecsh "dial" (phone-number-to-address number)))

(defun sms-send (sip-address message)
  ;; TODO add "chat" command to linphonecsh
  (linphonecsh "generic"
               (format nil "chat ~A ~A"
                       sip-address message)))

(in-package :stumpwm)

(defcommand sip-call-selection () ()
  (let* ((clipboard (get-x-selection nil :clipboard)))
    (sip:call clipboard)))

(defcommand sip-call-number (number) ((:string "Enter number: "))
  (assert (not (string-blank-p number)))
  (sip:call number))

(defcommand sip-call-contact (contact-number)
    ((:contact-number "Enter contact to call: "))
  (assert contact-number)
  (sip:call contact-number))

(defcommand sip-call-terminate () ()
  (sip:linphonecsh "generic" "terminate"))

(defcommand sip-call-dtmf (numbers) ((:string "enter DTMF tones to send: "))
  (sip:linphonecsh "generic" numbers))

(defcommand sip-call-mute () ()
  (sip:linphonecsh "generic" "mute"))

(defcommand sip-call-unmute () ()
  (sip:linphonecsh "generic" "unmute"))

(defcommand sip-call-answer () ()
  (sip:linphonecsh "generic" "answer"))

(defcommand sip-sms-send-number (number message)
    ((:string "Enter number: ") (:string "Enter SMS message: "))
  (assert (not (string-blank-p number)))
  (sip:sms-send (sip:phone-number-to-address number)
                message))

(defcommand sip-sms-send-selection (message)
    ((:string "Enter SMS message: "))
  (let* ((clipboard (get-x-selection nil :clipboard)))
    (sip-sms-send-number clipboard message)))

(defcommand sip-sms-send-contact (contact-number message)
    ((:contact-number "Enter contact to sms-send: ")
     (:string "Enter SMS message: "))
  (assert contact-number)
  (sip-sms-send-number contact-number message))

(defcommand sip-init () ()
  (sip:linphonecsh "init" "-c" (namestring (truename #P"~/.linphonerc"))))

(defcommand sip-exit () ()
  (sip:linphonecsh "exit"))
