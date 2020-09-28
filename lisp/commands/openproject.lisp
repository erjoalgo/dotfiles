(defpackage :openproject-client
  (:use :cl)
  (:export #:request #:create-work-package
           #:find-project
           #:find-user))

(in-package :openproject-client)

(defvar *debug-url* nil)

(defmacro drakma-http-request-or-error (url &rest args)
  `(multiple-value-bind (body status-code)
       (drakma:http-request ,url ,@args)
     (if (= (floor status-code 100) 2)
         ;; 2xx
         body
         (statusor:make-error
          (format nil "non-2xx status code: ~A on url ~A: ~A"
                  status-code ,url
                  (if (vectorp body)
                      (babel:octets-to-string body)
                      body))))))

(defun request (path &key method json content-type (url-encoder #'drakma:url-encode))
  (statusor:if-let-ok nil
                      ((auth (statusor:nil-to-error
                              (authinfo:get-by :app "openproject-personal")))
                       (machine (authinfo:alist-get-or-error :machine auth))
                       (scheme (or (authinfo:alist-get :scheme auth) "https"))
                       (api-key (authinfo:alist-get-or-error :apikey auth))
                       (url (format nil "~A~A"
                                    (or *debug-url* (format nil "~A://~A" scheme machine))
                                    path))
                       (content (when json (json:encode-json-to-string json)))
                       (content-type (or content-type (if content "application/hal+json")))
                       (resp-raw
                        (drakma-http-request-or-error
                         url :method (or method (if content :post :get))
                         :basic-authorization (list "apikey" api-key)
                         :content content
                         :content-type content-type
                         :DECODE-CONTENT t
                         :url-encoder url-encoder))
                       (resp-string (babel:octets-to-string resp-raw)))
                      (json:decode-json-from-string resp-string)))



(defun find-project (project-name)
  (statusor:if-let-ok nil
                      ((projects
                        (openproject-client:request "/api/v3/projects"))
                       (filtered-projects
                        (remove-if-not
                         (lambda (project)
                           (equal (access:access project :name) project-name))
                         (access:accesses projects :--EMBEDDED :ELEMENTS)))
                       (project (cond
                                  ((null filtered-projects)
                                   (statusor:make-error
                                    (format nil "no projects named ~A" project-name)))
                                  ((cdr filtered-projects)
                                   (statusor:make-error
                                    (format nil "more than one project named ~A" project-name)))
                                  (t (car filtered-projects)))))
                      project))

(defun find-user (email)
  (statusor:if-let-ok nil
                      ((users
                        (openproject-client:request "/api/v3/users"))
                       (filtered-users
                        (remove-if-not
                         (lambda (project)
                           (equal (access:access project :email) email))
                         (access:accesses users :--EMBEDDED :ELEMENTS)))
                       (project (cond
                                  ((null filtered-users)
                                   (statusor:make-error
                                    (format nil "no users with email ~A" project-name)))
                                  ((cdr filtered-users)
                                   (statusor:make-error
                                    (format nil "more than one project with email ~A"
                                            project-name)))
                                  (t (car filtered-users)))))
                      project))

(defun form-remove-null-links (form-json)
  (let* ((form-copy (copy-list form-json))
         (non-null-links
          (remove-if-not (lambda (link)
                           (access:accesses (cdr link) :href))
                         (access:accesses form-copy :--LINKS))))
    (setf (access:accesses form-copy :--LINKS) non-null-links)
    form-copy))


(defun create-work-package (subject &key
                                      (description "")
                                      (project-name ""))
  ;; TODO make drakma use quri instead of puri, which understands this valid URL
  ;; '(statusor:error-to-signal
  ;;   (openproject-client:request
  ;;    (format nil "/api/v3/projects?filters=[~A]"
  ;;     (cl-json:encode-json-alist-to-string
  ;;      `(("name_and_identifier" . (("operator" . "=")
  ;;                                  ("values" . ("personal")))))))
  ;;    :url-encoder #'quri:url-encode))

  (statusor:if-let-ok nil
                      ((project (find-project project-name))
                       (post-data
                        `(("subject" . ,subject)
                          ("description" .
                                         (("format" . "textile")
                                          ("raw" . ,description)))
                          ("_links" .
                                    (
                                     ("type" . (("href" . "/api/v3/types/1")))
                                     ("status" . (("href" . "/api/v3/statuses/1")))
                                     ("priority" . (("href" . "/api/v3/priorities/8")))
                                     ("project". ,(access:accesses project :--LINKS :SELF))))))
                       (filled-form (request "/api/v3/work_packages/form"
                                             :json post-data
                                             :content-type "application/json")))
                      (request "/api/v3/work_packages"
                               :json (form-remove-null-links
                                      (access:accesses filled-form :--EMBEDDED :PAYLOAD))
                               :method :post
                               :content-type "application/json")))

;; (setf *debug-url* "http://localhost:1234")

;; TODO move openproject to its own system, move out config below
(in-package :stumpwm)
(defcommand openproject-create-personal-task (subject) ((:string "enter task subject: "))
  (statusor:if-let-ok (err (message "error creating task: ~A" err))
                      ((resp (openproject-client:create-work-package
                              subject
                              :description ""
                              :project-name "Personal")))
                      (message "created successfully: ~A"
                               (access:accesses resp :--LINKS :SELF :HREF))))
