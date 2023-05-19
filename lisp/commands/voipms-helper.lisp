(in-package :voipms)

(defparameter *host-account-username* (UIOP/OS:HOSTNAME))
(defparameter *authinfo-machine-value* "voip.ms-linphonerc")

(defparameter *voipms-url-callback* nil)
(defparameter *never-to-cancel-dids* nil)
(defparameter *default-pop* nil)
(defparameter *default-forward-email* nil)
(defparameter *default-forward-number* nil)
(defparameter *default-accounts-to-update* nil)

(defun voipms-get-auth (&key maybe-prompt)
  (let ((info (or (authinfo:get-by-machine "voip.ms")
                  (when maybe-prompt
                    (authinfo:persist-authinfo-line
                     '((:name "machine" :value "voip.ms")
                       (:name "login" :prompt "enter email: ")
                       (:name "password")))))))
    (voipms:make-voipms-auth
     :username (voipms::alist-get :LOGIN info)
     :password (voipms::alist-get :PASSWORD info))))

(defparameter *auth* (voipms-get-auth))

(let ((secrets
        (make-pathname :name "voipms-secrets"
                       :type "lisp"
                       :defaults (or *load-truename* *compile-file-truename*
                                     *default-pathname-defaults*))))
  (when (probe-file secrets)
    (format t "loading secrets from ~A~%" secrets)
    (load secrets)))

(defun cancel-old-dids (auth &optional except)
  (loop
    with skip-cancel = (append except *never-to-cancel-dids*)
    with dids = (voipms::alist-get :DIDS (voipms::get-dids-info auth))
    for did in dids
    as did-number = (voipms::alist-get :DID did)
    unless (member did-number skip-cancel :test #'equal)
      do (progn (format t "cancelling ~A~%" did-number)
                (voipms::cancel-did auth :did did-number))))

(defun order-new-did (auth &key
                             (state "CA")
                             sip-account
                             (pop *default-pop*)
                             (email *default-forward-email* )
                             (fwd *default-forward-number*)
                             (dialtime-secs 60)
                             (test nil)
                             (billing-per-minute-p t)
                             ratecenter)
  (let* ((dids (voipms::get-dids-usa auth
                                     :state state
                                     :ratecenter ratecenter))
         (did (car (voipms::alist-get :DIDS dids)))
         (did-number (voipms::alist-get :DID did))
         (order-args (list
                      :did did-number
                      :routing (format nil "account:~A" sip-account)
                      :pop pop
                      :dialtime (format nil "~D" dialtime-secs)
                      :cnam "0"
                      :billing_type (if billing-per-minute-p "1" "2")
                      :test (if test "1" "0"))))
    (format t "ordering new did: ~A: ~A...~%" did-number order-args)
    (apply #'voipms::order-did auth order-args)
    (format t "ordered new did: ~A!~%" did-number)
    (format t "updating sms settings for did: ~A!...~%" did-number)
    (voipms::set-sms
     auth
     :did did-number
     :enable "1"
     :email_enabled (if email "1" "0")
     :email_address email
     :sms_forward_enable (if fwd "1" "0")
     :sms_forward fwd
     :url_callback_enable "1"
     :url_callback_retry "1"
     :url_callback *voipms-url-callback*)
    did))

(defun update-caller-id (auth did-number)
  (loop for account in *default-accounts-to-update*
        do (format t "updating account ~A to use did ~A~%" account did-number)
        do (voipms::set-sub-account-sparse auth
                                           account
                                           :callerid-number did-number)))

'(defun did-is-easy-to-remember (did)
  (loop
    with counts = (make-array '(10) :initial-element 0)
    with max = 0
    for dig across did
    as num = (- (char-code dig) (char-code #\0))
    do (setf max (max max (incf (aref counts num))))
    finally (return max)))

(defun cycle-did ()
  (let* ((did (order-new-did *auth*))
         (did-number (alist-get :DID did)))
    (update-caller-id *auth* did-number)
    '(cancel-old-dids *auth* (list did-number))))

(defun current-did (&key (account (car *default-accounts-to-update*)))
  (let ((account (car
                  (alist-get :ACCOUNTS (get-sub-accounts *auth* :account account)))))
    (alist-get :CALLERID-NUMBER account)))

(defun genpasswd (&key (len 13) (charset-regexp "[a-zA-Z0-9_-]"))
  (with-open-file (stream "/dev/random" :element-type '(unsigned-byte 8))
    (loop with pass = (make-string len)
          for i below len
          as c = (loop as code = (read-byte stream)
                       as c = (code-char code)
                       while (not (ppcre:scan charset-regexp
                                              (format nil "~C" c)))
                       finally (return c))
          do (setf (aref pass i) c)
          finally (return pass))))

(defun select-did (auth)
  (selcand:select
   :candidates (voipms::alist-get :DIDS (voipms::get-dids-info auth))
   :prompt "select caller id phone number (DID): "
   :stringify-fn (lambda (did) (voipms::alist-get :DID did))))

(defun create-host-linphone-subacount (auth
                                       &key
                                         (user *host-account-username*)
                                         (persist-authinfo t))
  (let* ((password (genpasswd))
         (did (select-did auth))
         (acc (voipms::create-sub-account
               auth
               :username user
               :protocol "1" ;; "SIP"
               :description (format nil "autogenerated client for host ~A" user)
               :auth_type "1" ;; "User/Password Authentication"
               :password password
               :device_type "2" ;; "ATA device, IP Phone or Softphone"
               :lock_international "0" ;; "International Calls Enabled"
               :international_route "1" ;; "Value", not "Premium"
               :music_on_hold "coffee_and_sunrise"
               :dtmf_mode "auto"
               :nat "yes"
               :allowed_codecs "all"
               :callerid_number (voipms::alist-get :DID did)))
         (account (voipms::alist-get :ACCOUNT acc)))
    (when persist-authinfo
      (authinfo:persist-authinfo-line
       `((:name "machine" :value *authinfo-machine-value*)
         (:name "login" :value ,account)
         (:name "password" :value ,password))))
    acc))

(defun find-host-account (auth &key (hostname *host-account-username*))
  (let (names)
    (loop
      for acc in (voipms::alist-get
                  :ACCOUNTS (voipms::get-sub-accounts auth))
      as name = (voipms::alist-get :ACCOUNT acc)
      do (push name names)
        thereis (ppcre:register-groups-bind (username) ("[0-9]+_(.*)" name)
                  (when (ppcre:scan
                         (format nil "^~A" (ppcre:quote-meta-chars username))
                         hostname)
                    acc)))))

(defun change-current-caller-id (auth)
  (let ((acc (find-host-account auth))
        (did-number (select-did-number auth)))
    (format t "DDEBUG voipms-helper.lisp dp71: value of acc: ~A~%" acc)
    (voipms::set-sub-account-sparse
     auth
     (voipms::alist-get :ACCOUNT acc)
     :callerid-number (voipms::alist-get :DID  did-number))))
