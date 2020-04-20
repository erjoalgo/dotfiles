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
   #:linphonec-init
   #:linphonec-started-p
   #:linphonec-restart
   #:linphone-call-state))
(in-package :sip)

(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (stumpwm:message-wrapped "linphonecsh ~{~A~^ ~}" args)
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
  ;; Call states
  ;; Id |            Destination              |      State      |    Flags   |
  ;; ------------------------------------------------------------------------
  ;; 6  | sip:user@example.com     | OutgoingEarlyMedia |
  ;; 7  | sip:user@example.com       | IncomingReceived |
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

(defun linphonec-init ()
  (sip:linphonecsh "init" "-c" (namestring (truename #P"~/.linphonerc"))))

(defun linphonec-started-p ()
  ;; TODO actually check registration status
  (multiple-value-bind (retcode output)
      (stumpwm::run-command-retcode-output "linphonecsh" (list "generic" "help"))
    (declare (ignore output))
    (zerop retcode)))

(defun linphonec-restart ()
  (stumpwm::run-command-retcode-output "pkill" (list "linphone"))
  (linphonec-init))

(in-package :stumpwm)

;; contact (either call or text)
(defun sip-contact (number)
  (let ((choice (selcand:select
                 :hints-candidates `(("c" . :call)
                                     ("t" . :text)
                                     ("e" . :email))
                 :read-char-if-possible t
                 :display-candidates t)))
    (case choice
      (:call (message "calling ~A" number) (sip:call number))
      (:text (let ((cmd (format nil "emacssip ~A" number)))
               (message "invoking ~A" cmd)
               (run-shell-command cmd)))
      (:email (error "email not implemented"))
      (t (error "Unknown choice: ~A" choice)))))

(defcommand sip-contact-selection () ()
  (let* ((clipboard (get-x-selection nil :clipboard)))
    (sip-contact clipboard)))

(defcommand sip-contact-number (number) ((:non-blank-string "Enter number: "))
  (sip-contact number))

(defcommand sip-contact-contact (contact-number)
    ((:contact-number "Select contact: "))
  (sip-contact contact-number))

;; call

(defcommand sip-call-terminate () ()
  (sip:linphonecsh "generic" "terminate"))

(defcommand sip-call-dtmf (numbers) ((:non-blank-string "enter DTMF tones to send: "))
  (sip:linphonecsh "generic" numbers))

(defcommand sip-call-mute () ()
  (sip:linphonecsh "generic" "mute"))

(defcommand sip-call-unmute () ()
  (sip:linphonecsh "generic" "unmute"))

(defcommand sip-sms-send-number (number message)
    ((:non-blank-string "Enter number: ") (:non-blank-string "Enter SMS message: "))
  (sip:sms-send (sip:phone-number-to-address number)
                message))

(defcommand sip-call-answer () ()
  (let ((active-calls (remove-if-not
                       (lambda (call) (equal "IncomingReceived"
                                             (sip:linphone-call-state call)))
                       (sip:linphonecsh-active-calls))))
    (if (null active-calls)
        (error "no active calls found")
        (let* ((call (selcand:select
                      :candidates active-calls
                      :prompt "select call to answer: "
                      :stringify-fn #'sip:linphone-call-destination)))
          (if (null call)
              (error "no call selected")
              (let* ((call-id (sip:linphone-call-id call))
                     (command (format nil "answer ~D" call-id)))
                (sip:linphonecsh "generic" command)))))))

(defcommand sip-main () ()
  (let* ((clipboard-choice
          (format nil "clipboard: ~A"
                  (let ((clipboard (get-x-selection nil :clipboard)))
                    (subseq clipboard 0 (min 10 (length clipboard))))))
         (choice
          (selcand:select
           :hints-candidates `(("l" . ,clipboard-choice)
                               ("n" . :enter-number)
                               ("c" . :contact-selection))
           :read-char-if-possible t
           :display-candidates t)))
    (case choice
      (:enter-number (call-interactively "sip-contact-number"))
      (:contact-selection (call-interactively "sip-contact-contact"))
      (t (if (equal choice clipboard-choice)
             (call-interactively "sip-contact-selection")
             (error "Unknown choice: ~A" choice))))))

(defcommand sip-init () ()
  (sip:linphonec-init))

(defcommand sip-exit () ()
  (sip:linphonecsh "exit"))
