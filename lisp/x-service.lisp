(defpackage :x-service
  (:use :cl)
  (:export #:start)
  (:import-from :stumpwm :-> :->>))

(in-package :x-service)

(defvar *x-service* nil)

(defun start (port)
  "Start a service based on the config obtained by proxying all arguments make-config"
  (when (and *x-service*
             (or (null port) (hunchentoot:started-p *x-service*)))
    (hunchentoot:stop *x-service*)
    (setf *x-service* nil))
  (when port
    (setf *x-service*
          (make-instance 'hunchentoot:easy-acceptor
                         :port port
                         :access-log-destination nil))
    (hunchentoot:start *x-service*)))

(defmacro define-regexp-route (name (url-regexp &rest capture-names) docstring &body body)
  "A macro to define a handler `name' matching requests for `url-regexp'.
An optional list `capture-names' can be provided to capture path variables.
The capturing behavior is based on wrapping `ppcre:register-groups-bind'
"
  `(progn
     (defun ,name ()
       ,docstring
       (ppcre:register-groups-bind ,capture-names
           (,url-regexp (hunchentoot:script-name*))
         (handler-case (progn ,@body)
           (error (err)
             (setf (hunchentoot:return-code*)
                   hunchentoot:+HTTP-INTERNAL-SERVER-ERROR+)
             (format nil "x-service error: ~A" err)))))
     (push (hunchentoot:create-regex-dispatcher ,url-regexp ',name)
           hunchentoot:*dispatch-table*)))

(setf hunchentoot:*dispatch-table* nil)

(defun hunchentoot-post-data-or-err ()
  (cond
    ((not (eq (hunchentoot:request-method*) :post))
     (error "Request is not :post, but ~A" (hunchentoot:request-method*)))
    ((null (hunchentoot:raw-post-data))
     (error "Missing post data"))
    (t (-> (hunchentoot:raw-post-data)
          (babel:octets-to-string)))))

(define-regexp-route notify-handler ("/notify")
    "Issue a notification"
  (let* ((text (hunchentoot-post-data-or-err))
         (color (->> (hunchentoot:headers-in*)
                     (assoc :STUMPWM-MESSAGE-COLOR)
                     cdr)))
    (when color
      (setf text (stumpwm:message-colorize text color)))
    (stumpwm::message-wrapped "~A" text)
    ""))

;; (defalias url-browse url-launcher-browser-new-tab)

(define-regexp-route browse-handler ("/browse")
    "Browse to a URL"
  (let ((url (hunchentoot-post-data-or-err)))
    (format t "x-service: value of url: ~A~%" url)
    (stumpwm::url-launcher-browser-new-tab url)
    ""))

(define-regexp-route clipboard-handler ("/clipboard")
    "get/set clipboard contents"
  (case (hunchentoot:request-method*)
    (:post
     (let ((contents (hunchentoot-post-data-or-err)))
       (stumpwm:set-x-selection contents :clipboard)
       (stumpwm:set-x-selection contents :primary)
       ""))
    (:get (stumpwm:get-x-selection :clipboard))))

(define-regexp-route visible-window-pids-handler ("/visible-window-pids")
    "get a list of pids of windows that are visible"
  (->>
   (stumpwm:group-windows (stumpwm:current-group))
   (remove-if-not
    #'stumpwm:window-visible-p)
   (mapcar #'stumpwm:window-pid)
   (format nil "~{~A~^~%~}")))

;; (service-start 1959)
