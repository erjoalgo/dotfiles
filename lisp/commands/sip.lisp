(defpackage :sip
  (:use :cl)
  (:export
   #:call
   #:linphonecsh
   #:linphonecsh-sync
   #:sms-send
   #:phone-number-to-address
   #:linphonecsh-active-calls
   #:linphone-call-id
   #:linphone-call-destination
   #:linphone-call-state))
(in-package :sip)

(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (stumpwm:message "running: linphonecsh ~{~A~^ ~}" args)
  (stumpwm::run-command-async "linphonecsh" args nil t))

(defun linphonecsh-sync (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (stumpwm:message "running: linphonecsh ~{~A~^ ~}" args)
  (multiple-value-bind (retcode output)
      (stumpwm::run-command-retcode-output "linphonecsh" args)
    (if (zerop retcode)
        output
        (error "non-zero exit status: ~A ~A" retcode output))))

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

(defun linphonecsh-active-calls ()
  (let ((output (linphonecsh-sync "generic" "calls")))
    (linphonecsh-parse-active-calls output)))

(defstruct linphone-call
  id destination state flags)

(defun linphonecsh-parse-active-calls (output)
  (let* ((col (format nil " *([^|~%]*?) *"))
         (regexp (format nil "(?m)^~{~A~^[|]~}~%" (list col col col col)))
         calls)
    (format t regexp)
    (cl-ppcre:do-register-groups (id destination state flags) (regexp output nil :sharedp t)
      (push (make-linphone-call :id id
                                :destination destination
                                :state state
                                :flags flags)
            calls))
    calls))

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

(defcommand sip-call-answer () ()
  (let ((active-calls (remove-if-not
                       (lambda (call) (equal "IncomingReceived"
                                             (sip:linphone-call-state call)))
                       (sip:linphonecsh-active-calls))))
    (if (null active-calls)
        (error "no active calls found")
        (let* ((call (selcand:select
                      active-calls
                      "select call to answer: "
                      #'sip:linphone-call-destination
                      t)))
          (if (null call)
              (error "no call selected")
              (let* ((call-id (sip:linphone-call-id call))
                     (command (format nil "answer ~D" call-id)))
                (sip:linphonecsh "generic" command)))))))

(defcommand sip-init () ()
  (sip:linphonecsh "init" "-c" (namestring (truename #P"~/.linphonerc"))))

(defcommand sip-exit () ()
  (sip:linphonecsh "exit"))
