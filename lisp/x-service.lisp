(defvar *x-service* nil)

(defun service-start (port)
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

(defparameter *message-colors*
  '(:black
    :red
    :green
    :yellow
    :blue
    :magenta
    :cyan
    :white)
  "Message colors.")

(defun message-colorize (msg color)
  (let* ((color-sym (-> color princ-to-string string-upcase (intern :keyword)))
         (idx (position color-sym *message-colors*)))
    (if idx
        (format nil "^~D~A^*" idx msg)
        (error "no such color: ~A. choices: ~A"
               color
               *message-colors*))))

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
      (setf text (message-colorize text color)))
    (message-wrapped "~A" text)
    ""))

;; (defalias url-browse url-launcher-browser-new-tab)

(define-regexp-route browse-handler ("/browse")
    "Browse to a URL"
  (let ((url (hunchentoot-post-data-or-err)))
    (format t "x-service: value of url: ~A~%" url)
    (url-launcher-browser-new-tab url)
    ""))

(define-regexp-route clipboard-handler ("/clipboard")
    "get/set clipboard contents"
  (case (hunchentoot:request-method*)
    (:post
     (let ((contents (hunchentoot-post-data-or-err)))
       (set-x-selection contents :clipboard)
       (set-x-selection contents :primary)
       ""))
    (:get (get-x-selection :clipboard))))

(define-regexp-route visible-window-pids-handler ("/visible-window-pids")
    "get a list of pids of windows that are visible"
  (->>
   (group-windows (current-group))
   (remove-if-not
    #'window-visible-p)
   (mapcar #'window-pid)
   (format nil "~{~A~^~%~}")))

(service-start 1959)
