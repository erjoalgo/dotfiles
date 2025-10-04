(defpackage :x-service
  (:use :cl)
  (:export #:start #:define-regexp-route)
  (:import-from :stumpwm :-> :->>))

(in-package :x-service)

(defvar *x-service* nil)
(defvar *x-service-debugging* nil)

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
             (stumpwm:message "x-service error: ~A" err)))))
     (push (hunchentoot:create-regex-dispatcher ,url-regexp ',name)
           hunchentoot:*dispatch-table*)))

(setf hunchentoot:*dispatch-table* nil)

(defun hunchentoot-post-data-or-err ()
  (cond
    ((not (eq (hunchentoot:request-method*) :post))
     (error "Request should be :post, but is ~A" (hunchentoot:request-method*)))
    ((null (hunchentoot:raw-post-data))
     "")
    (t (-> (hunchentoot:raw-post-data)
           (babel:octets-to-string)))))

(defun read-header (header-sym)
  (->> (hunchentoot:headers-in*)
       (assoc header-sym)
       cdr))

(define-regexp-route notify-handler ("/notify")
                     "Issue a notification"
  (let* ((text (hunchentoot-post-data-or-err))
         (color (read-header :STUMPWM-MESSAGE-COLOR)))
    (when color
      (setf text (stumpwm:message-colorize text color)))
    (stumpwm::message-wrapped "~A" text)
    ""))

(define-regexp-route browse-handler ("/browse")
                     "Browse to a URL"
  (let ((url (hunchentoot-post-data-or-err))
        (raise-browser-window-p
          (string-equal (read-header :STUMPWM-RAISE-BROWSER-WINDOW) "TRUE")))
    (format t "x-service: value of url: ~A~%" url)
    (stumpwm::x-www-browser url raise-browser-window-p) ""))

(define-regexp-route clipboard-handler ("/clipboard")
                     "get/set clipboard contents"
  (case (hunchentoot:request-method*)
    (:post
     (let ((contents (hunchentoot-post-data-or-err))
           (notify (equal (read-header :STUMPWM-NOTIFY) "true")))
       (stumpwm:set-x-selection contents :clipboard)
       (stumpwm:set-x-selection contents :primary)
       (when notify
         (stumpwm::message-wrapped "copied: ~A" contents))
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

(define-regexp-route read-char-handler ("/read-char")
                     "Read a single character"
  (let ((prompt
          (read-header :STUMPWM-PROMPT)))
    (when prompt
      (stumpwm:message-wrapped (format nil "~A " prompt)))
    (prog1
        (format nil "~C" (stumpwm:read-one-char (stumpwm:current-screen)))
      (stumpwm::unmap-all-message-windows))))

(define-regexp-route read-line-handler ("/read-line")
                     "Read a line"
  (let ((prompt
          (read-header :STUMPWM-PROMPT))
        (completions
          (read-header :STUMPWM-COMPLETIONS))
        (require-match
          (read-header :STUMPWM-REQUIRE-MATCH)))
    (or (stumpwm:read-one-line
         (stumpwm:current-screen) (format nil "~A " prompt)
         :completions completions
         :require-match require-match)
        (throw 'error "Abort."))))

(define-regexp-route search-handler ("/search")
                     "Web search"
  (let* ((query (hunchentoot-post-data-or-err))
         (engine-header (read-header :ENGINE))
         (engine-letter-header (read-header :ENGINE-LETTER))
         (engine (if engine-letter-header
                     (progn
                       (assert (eq (length engine-letter-header) 1))
                       (or
                        (stumpwm::search-engine-find-by-key engine-letter-header)
                        (error "No engine found for key ~A" engine-letter-header)))
                     (stumpwm::search-engine-find-by-id (or engine-header "ddg")))))
    (stumpwm::search-engine-search-noninteractive query engine)))

(define-regexp-route run-handler ("/run")
                     "Run command"
  (let ((command (hunchentoot-post-data-or-err)))
    (stumpwm::eval-command command t)))

(define-regexp-route beep-handler ("/beep")
                     "Beep"
  (let* ((freq (read-header :STUMPWM-BEEP-FREQ))
         (duration-secs (read-header :STUMPWM-BEEP-DURATION-SECS))
         kwargs)
    (when freq
      (push `(:freq ,(READ-FROM-STRING freq)) kwargs))
    (when duration-secs
      (push `(:duration-secs ,(READ-FROM-STRING duration-secs)) kwargs))
    (apply #'stumpwm::beep-fn (loop for kv in kwargs
                                    append kv))
    ""))

(define-regexp-route lock-handler ("/lock")
                     "Lock the display"
  (let ((addr (hunchentoot:real-remote-addr)))
    (stumpwm:message-wrapped
     "x-service received /lock request from ~A" addr))
  (let ((caller (read-header :CALLER)))
    (if caller
        (progn
          (stumpwm::screen-lock (format nil "x-service: ~A" caller))
          "ok")
        (progn
          (let ((err-msg (format nil "x-service refuses to lock screen: missing the CALLER header")))
            (setf (hunchentoot:return-code*)
                  hunchentoot:+HTTP-BAD-REQUEST+)
            (stumpwm:message-wrapped "~A" err-msg)
            err-msg)))))

(define-regexp-route url-launcher-put ("/url-launcher-put")
                     "add an entry to url-launcher"
  (let* ((url (read-header :URL))
         (alias (read-header :ALIAS)))
    (stumpwm:message-wrapped (format nil "URL: ~A" url))
    (stumpwm:message-wrapped (format nil "ALIAS: ~A" ALIAS))
    (stumpwm::launcher-append-url alias url)))

(define-regexp-route raise-window ("/raise-window")
                     "raise the window matching the given regexp"
  (let* ((regexp (read-header :REGEXP))
         (win (stumpwm::find-window-by-regexp regexp)))
    (if win
        (stumpwm::raise-window win)
        (let ((err-msg (format nil "no such window matching '~A'~%: ~{~A~^~%~}"
                               regexp
                               (loop for win in (stumpwm::list-windows (stumpwm::current-screen))
                                     collect (stumpwm::window-title win)))))
          (stumpwm::message "^1~A*" err-msg)
          (setf (hunchentoot:return-code*)
                hunchentoot:+HTTP-NOT-FOUND+)
          err-msg))))

(define-regexp-route desktop-group-number-for-window ("/desktop-group-number")
                     "return the current desktop's group number"
  (let* ((pid-str (read-header :STUMPWM-WIN-PID)))
    (assert (ppcre:scan "^[0-9]+$" pid-str) nil "missing window pid: ~A" pid-str)
    (or
     (loop
       with pid = (parse-number:parse-number pid-str)
       for g in (stumpwm::sort-groups (stumpwm:current-screen))
       for group-index from 1
         thereis
         (loop for win in (stumpwm:group-windows g)
                 thereis (when (= pid (stumpwm:window-pid win))
                           (setf (hunchentoot:return-code*)
                                 hunchentoot:+HTTP-OK+)
                           (format nil "~A" group-index))))
     (let ((err-msg (format nil "no window found with pid ~A" pid-str)))
       (stumpwm::message "^1~A*" err-msg)
       (setf (hunchentoot:return-code*)
             hunchentoot:+HTTP-NOT-FOUND+)))))

;; (x-service:start 1959)
