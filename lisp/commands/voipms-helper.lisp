(in-package :voipms)

(defparameter *voipms-url-callback* nil)
(defparameter *never-to-cancel-dids* nil)
(defparameter *default-pop* nil)
(defparameter *default-forward-email* nil)
(defparameter *default-forward-number* nil)
(defparameter *default-routing-group* nil)
(defparameter *default-accounts-to-update* nil)
(defparameter *auth*
  (let ((info (authinfo:get-by-machine "voip.ms")))
    (voipms:make-voipms-auth
     :username (voipms::alist-get :LOGIN info)
     :password (voipms::alist-get :PASSWORD info))))

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
                             (routing-group *default-routing-group*)
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
    (cancel-old-dids *auth* (list did-number))))

(defun current-did (&key (account (car *default-accounts-to-update*)))
  (let ((account (car
                  (alist-get :ACCOUNTS (get-sub-accounts *auth* :account account)))))
    (alist-get :CALLERID-NUMBER account)))
