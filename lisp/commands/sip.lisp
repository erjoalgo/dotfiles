(defpackage :sip
  (:use :cl)
  (:export
   #:call
   #:linphonecsh
   #:linphonecsh-sync
   #:sms-send
   #:phone-number-to-address
   #:sip-sanitize-phone-number
   #:linphonecsh-active-calls
   #:linphonecsh-proxies
   #:linphone-proxy-index
   #:linphone-proxy-identity
   #:linphonecsh-set-default-proxy-index
   #:linphonecsh-current-default-proxy
   #:linphone-call-destination
   #:linphone-call-id
   #:linphone-call-destination
   #:linphonec-init
   #:linphonec-started-p
   #:linphonec-kill
   #:linphonec-restart
   #:linphone-inhibit-command-echo
   #:linphone-call-state))
(in-package :sip)

(defun linphonecsh (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (stumpwm:message-wrapped "linphonecsh ~{~A~^ ~}" args)
  (stumpwm::run-command-async "linphonecsh" args nil t))

(defvar linphone-inhibit-command-echo nil)

(defun linphonecsh-sync (&rest args)
  "Execute a linphonec command via linphonecsh."
  ;; TODO check if "linphonecsh init" needs to be called
  (unless linphone-inhibit-command-echo
    (stumpwm:message "running: linphonecsh ~{~A~^ ~}" args))
  (multiple-value-bind (retcode output)
      (stumpwm::run-command-retcode-output "linphonecsh" args)
    (if (zerop retcode)
        output
        (error "non-zero exit status: ~A ~A" retcode output))))

(defun sip-current-identity ()
  (let* ((output (linphonecsh-sync "generic" "proxy show default")))
    (or
     (ppcre:register-groups-bind (user host) ("identity: sip:(.*)@(.*).*" output)
       (list user host))
     (error "unable to determine default proxy: ~A" output))))

(defun sip-default-host ()
  (second (sip-current-identity))
  "sanjose2.voip.ms"
  )

(defun sip-sanitize-phone-number (text)
  (let* ((no-alpha
          (sip-alpha-text-to-phone-number text))
        (number-clean (ppcre:regex-replace-all "[^0-9]|^[+]1" no-alpha "")))
    number-clean))

(defun phone-number-to-address (number &key (sip-host (sip-default-host)))
  (let* ((number-clean (sip-sanitize-phone-number number))
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

(defstruct linphone-proxy
  index identity)

(defun linphonecsh-parse-proxies (text)
  ;; ****** Proxy 0 - this is the default one - *******
  ;; sip address: <sip:sanjose2.voip.ms>
  ;; route:
  ;; identity: sip:263366_xxxxxxxx@sanjose2.voip.ms
  ;; register: yes
  ;; expires: 3600
  ;; registered: yes
  ;; ****** Proxy 1 *******
  ;; sip address: <sip:sanjose2.voip.ms>
  ;; route:
  ;; identity: sip:263366_yyyyyyyy@sanjose2.voip.ms
  ;; register: yes
  ;; expires: 3600
  ;; registered: yes
  (let (proxies)
    (cl-ppcre:do-register-groups (index identity)
        ((format nil "(?s)[*]{6} Proxy ([0-9]+).*?[*]{6}.*?identity: (.*?)~%")
         text nil :sharedp t)
      (push (make-linphone-proxy :index index :identity identity) proxies))
    proxies))

(defun linphonecsh-proxies ()
  (let ((output (linphonecsh-sync "generic" "proxy list")))
    (linphonecsh-parse-proxies output)))

(defun linphonecsh-set-default-proxy-index (index)
  (linphonecsh-sync "generic" (format nil "proxy use ~D" index)))

(defun linphonecsh-current-default-proxy ()
  (let ((output (linphonecsh-sync "generic" "proxy show default")))
    output))

(defun linphonec-init ()
  (linphonec-kill)
  (sip:linphonecsh "init" "-c" (namestring (truename #P"~/.linphonerc"))))

(defun linphonec-started-p ()
  ;; TODO actually check registration status
  (multiple-value-bind (retcode output)
      (stumpwm::run-command-retcode-output "linphonecsh" (list "generic" "help"))
    (declare (ignore output))
    (zerop retcode)))

(defun linphonec-kill ()
  (stumpwm::run-command-retcode-output "sudo" (list "pkill" "-9" "linphone")))

(defun linphonec-restart ()
  (linphonec-kill)
  (linphonec-init))

(defmacro sip-alpha-to-digit (char)
  (let ((char-sym (gensym "CHAR-")))
    `(let ((,char-sym ,char))
       (case (if (characterp ,char-sym)
                 ,char-sym
                 (progn
                   (assert (and (stringp ,char-sym)
                                (= 1 (length ,char-sym))))
                   (char ,char-sym 0)))
         ,@(loop
              for (chars . digit) in
                `(("abc" . 2) ("def" . 3)
                  ("gih" . 4) ("jkl" . 5) ("mno" . 6)
                  ("pqrs" . 7) ("tuv" . 8) ("xyz" . 9))
              append (loop for c across
                          (concatenate 'string
                                       chars (string-upcase chars))
                        collect `(,c ,digit)))
         (t (error "No dial-pad digit for char ~A"
                   ,char-sym))))))

(defun sip-alpha-text-to-phone-number (text)
  (loop
     with as-number = (make-string
                       (length text)
                       :initial-element #\Space)
     for c across text
     for i from 0
     as cc = (cond
               ((alpha-char-p c)
                (code-char
                 (+ (char-code #\0)
                    (sip-alpha-to-digit c))))
               ((digit-char-p c) c)
               (t nil))
     when cc
     do (setf (aref as-number i) cc)
     finally (return as-number)))

(in-package :stumpwm)

;; contact (either call or text)
(defun sip-contact (number)
  (let ((choice (selcand:select
                 :hints-candidates `(("c" . :call)
                                     ("t" . :text)
                                     ("e" . :email))
                 :read-char-if-possible t
                 :display-candidates :include-values))
        (number-clean (sip:sip-sanitize-phone-number number)))
    (case choice
      (:call (message "calling ~A" number) (sip:call number-clean))
      (:text (let ((cmd (format nil "emacs-sip ~A" number-clean)))
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

(defcommand sip-select-default-proxy () ()
  (with-message-queuing t
    (sip-show-current-default-proxy)
    (let* ((proxies (let ((sip:linphone-inhibit-command-echo t))
                      (sip:linphonecsh-proxies)))
           (selected (selcand:select
                      :candidates proxies
                      :prompt "select proxy: "
                      :stringify-fn #'sip:linphone-proxy-identity)))
      (assert selected)
      (sip:linphonecsh-set-default-proxy-index (sip:linphone-proxy-index selected)))))

(defcommand sip-show-current-default-proxy () ()
  (message "current proxy:~%a~%~A" (sip:linphonecsh-current-default-proxy)))

(defcommand sip-echo-test () ()
  (sip:call "4443"))

(defcommand espeak (text) ((:string "enter text to speak: "))
  (stumpwm::run-command-async "espeak" (list text) nil t))

(defcommand espeak-spell (text) ((:string "enter text to spell: "))
  (spell-word text))

(defcommand sip-main () ()
  (let* ((clipboard-choice
          (format nil "clipboard: ~A"
                  (let ((clipboard (get-x-selection nil :clipboard)))
                    (subseq clipboard 0 (min 15 (length clipboard))))))
         (choice
          (selcand:select
           :hints-candidates `(("l" . ,clipboard-choice)
                               ("n" . :enter-number)
                               ("c" . :contact-selection)
                               ("e" . :echo-test)
                               ("d" . :dtmf)
                               ("s" . :espeak)
                               ("S" . :espeak-spell)
                               ("t" . :call-terminate)
                               ("a" . :call-answer)
                               ("p" . :proxy-select)
                               ("r" . :linphonec-restart)
                               ("m" . :sip-call-mute)
                               ("M" . :sip-call-unmute))
           :read-char-if-possible t
           :display-candidates :include-values)))
    (case choice
      (:enter-number (call-interactively "sip-contact-number"))
      (:contact-selection (call-interactively "sip-contact-contact"))
      (:echo-test (call-interactively "sip-echo-test"))
      (:dtmf (call-interactively "sip-call-dtmf"))
      (:espeak (call-interactively "espeak"))
      (:espeak-spell (call-interactively "espeak-spell"))
      (:call-terminate (call-interactively "sip-call-terminate"))
      (:call-answer (call-interactively "sip-call-answer"))
      (:proxy-select (call-interactively "sip-select-default-proxy"))
      (:linphonec-restart
       '(call-interactively "sip:linphonec-restart")
       (sip:linphonec-restart))
      (t (if (equal choice clipboard-choice)
             (call-interactively "sip-contact-selection")
             (call-interactively (symbol-name choice)))))))

(defcommand sip-init () ()
  (sip:linphonec-init))

(defcommand sip-exit () ()
  (sip:linphonecsh "exit"))
