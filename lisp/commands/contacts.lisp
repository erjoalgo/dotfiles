(defpackage :contacts
  (:use :cl)
  (:export
   #:contacts-load
   #:contact-name
   #:contact-emails
   #:contact-phones))
(in-package :contacts)

(ql:quickload 'cl-csv)

(defvar *contacts-csv* #P"~/Downloads/contacts.csv")

(defvar *contacts* nil)

(defstruct contact
  name
  emails
  phones)

;; 0 First Name 1 Middle Name 2 Last Name 3 Title 4 Suffix 5 Initials 6 Web Page 7 Gender
;; 8 Birthday 9 Anniversary 10 Location 11 Language 12 Internet Free Busy 13 Notes
;; 14 E-mail Address 15 E-mail 2 Address 16 E-mail 3 Address 17 Primary Phone 18 Home Phone
;; 19 Home Phone 2 20 Mobile Phone 21 Pager 22 Home Fax 23 Home Address 24 Home Street
;; 25 Home Street 2 26 Home Street 3 27 Home Address PO Box 28 Home City 29 Home State
;; 30 Home Postal Code 31 Home Country 32 Spouse 33 Children 34 Manager's Name
;; 35 Assistant's Name 36 Referred By 37 Company Main Phone 38 Business Phone 39 Business Phone 2
;; 40 Business Fax 41 Assistant's Phone 42 Company 43 Job Title 44 Department 45 Office Location
;; 46 Organizational ID Number 47 Profession 48 Account 49 Business Address 50 Business Street
;; 51 Business Street 2 52 Business Street 3 53 Business Address PO Box 54 Business City
;; 55 Business State 56 Business Postal Code 57 Business Country 58 Other Phone 59 Other Fax
;; 60 Other Address 61 Other Street 62 Other Street 2 63 Other Street 3 64 Other Address PO Box
;; 65 Other City 66 Other State 67 Other Postal Code 68 Other Country 69 Callback
;; 70 Car Phone 71 ISDN 72 Radio Phone 73 TTY/TDD Phone 74 Telex
;; 75 User 1 76 User 2 77 User 3 78 User 4 79 Keywords
;; 80 Mileage 81 Hobby 82 Billing Information 83 Directory Server 84 Sensitivity
;; 85 Priority 86 Private 87 Categories

(defun contacts-read (&key (contacts-csv *contacts-csv*))
  (labels ((extract-cols (row row-indices)
             (loop for idx in row-indices
                as val = (string-trim '(#\space) (nth idx row))
                when (not (zerop (length val)))
                collect val)))
    (cdr ;; skip the header row
     (cl-csv:read-csv contacts-csv
                     :map-fn
                     #'(lambda (row)
                         (make-contact
                          :name (format nil "~{~A~^ ~}"
                                        (extract-cols row '(3 0 1 2 4)))
                          :emails (extract-cols row '(14 15 16))
                          :phones (extract-cols row '(20 17 18 19 21 22))))))))

(stumpwm:define-stumpwm-type :contact (input prompt)
  (or (stumpwm:argument-pop input)
      (stumpwm::when-let*
          ((contact-name
            (stumpwm:completing-read (stumpwm:current-screen)
		                     prompt
                                     (mapcar #'contact-name *contacts*)))
           (contact (loop for contact in *contacts*
                       thereis (when (equal (contact-name contact)
                                            contact-name)
                                 contact))))
        contact)
      (throw 'error "Abort")))

(stumpwm:define-stumpwm-type :contact-number (input prompt)
  (or
   (stumpwm::when-let*
       ((contact
         (funcall (gethash :contact stumpwm::*command-type-hash*) input prompt))
        (phones (contact-phones contact))
        (number (selcand:select
                 phones
                 "select among multiple phone number: "
                 #'identity t)))
     number)
   (throw 'error "Abort")))

(stumpwm:define-stumpwm-type :contact (input prompt)
  (or (stumpwm:argument-pop input)
      (selcand:select *contacts* prompt #'contact-name nil t)
      (throw 'error "No contact selected (abort?)")))

(defun contacts-load ()
  (setq *contacts* (contacts-read)))

(x-service:define-regexp-route contacts-handler ("/contacts")
    "Display a list of contacts"
  (cl-who:with-html-output-to-string (str)
    (:table :border 3 :cellpadding 4
            (loop for contact in contacts::*contacts*
               as name = (contacts:contact-name contact)
               as emails = (contacts:contact-emails contact)
               as phones = (contacts:contact-phones contact)
               do
                 (cl-who:htm
                  (:tr
                   (:td
                    ;; :bgcolor "green"
                    (cl-who:fmt "~A" name))
                   (:td
                    (:ul
                     (loop for phone in phones
                        do (cl-who:htm
                            (:li
                             (:a :href
                                 (format nil "tel:~A"
                                         (ppcre:regex-replace-all "[^0-9]" phone ""))
                                 (cl-who:fmt "~A" phone)))))))
                   (:td
                    (:ul
                     (loop for email in emails
                        do (cl-who:htm
                            (:li
                             (:a :href (format nil "mailto:~A" email)
                                 (cl-who:fmt "~A" email))))))))))
            str)))

;; (contacts-load)
