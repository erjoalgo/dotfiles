(in-package :STUMPWM)

(defpackage #:url-launcher
  (:export
   #:launch-url
   #:launcher-append-url
   #:uri-encode))

(use-package :CL)

(defvar *webdav-server-info* nil)

(defun webdav-load-server-info ()
  ;; may be interactive
  (unless (authinfo:get-by :app "webdav")
    (authinfo:persist-authinfo-line
     `((:name "app" :value "webdav")
       (:name "machine" :value "webdav.erjoalgo.com" )
       (:name "login" :value "erjoalgo")
       (:name "password"))
     :no-prompt t))
  (statusor:if-let-ok nil
                      (
                       (auth (statusor:nil-to-error (authinfo:get-by :app "webdav")))
                       (machine (authinfo:alist-get-or-error :machine auth))
                       (scheme (or (authinfo:alist-get :scheme auth) "https"))
                       (port (authinfo:alist-get :port auth))
                       (url (format nil "~A://~A~A" scheme machine
                                    (if port (format nil ":~A" port) "")))
                       (user (authinfo:alist-get :login auth))
                       (password (authinfo:alist-get :password auth)))
                      (setf *webdav-server-info*
                            (cladaver:make-server-info :base-url url
                                                       :username user
                                                       :password password))))

;; note the trailing slash.
;; needed to allow merging additional pathname components
(defvar webdav-urls-prefix #P"/urls/")

(defun url-launcher-get-browser-current-url ()
  (mozrepl:chrome-get-url))

;; actually load from the file
(defparameter *url-command-rules*
  `(
    (".*[.]pdf$" "zathura")
    ("(^https?://.*|.*[.]html.*).*" ,#'x-www-browser)
    (".*[.](docx?|odt)$" "libreoffice")
    ("about:config" ,#'mozrepl:firefox-new-tab)
    ;; ("" ,#'mozrepl:firefox-new-tab)
    ("^file://.*" "file-open.el")
    ("^rtsp://.*" "rtsp-play.sh")))

(defun url-command (url)
  (loop for (regexp opener) in *url-command-rules*
          thereis (and (cl-ppcre:scan regexp url) opener)
        finally (return #'x-www-browser)))

(defvar *url-keys-cache* nil)

(defvar *url-values-cache* nil)

(defun url-launcher-list-url-keys (&key skip-cache)
  (if (and (not skip-cache) *url-keys-cache*)
      (prog1
          *url-keys-cache*
        (stumpwm::lparallel-future
         (url-launcher-list-url-keys :skip-cache t)))
      (setf *url-keys-cache*
            (statusor:error-to-signal
             (cladaver:ls *webdav-server-info* webdav-urls-prefix)))))

(defun url-launcher-cat-webdav-path (webdav-path &key skip-cache)
  (let ((val
          (and (not skip-cache)
               (cdr
                (assoc webdav-path *url-values-cache* :test #'equal)))))
    (if val
        (prog1 val
          (stumpwm::lparallel-future
           (url-launcher-cat-webdav-path webdav-path :skip-cache t)))
        (prog1
            (setf val (statusor:error-to-signal
                       (cladaver:cat *webdav-server-info* webdav-path)))
          (pushnew (cons webdav-path val) *url-values-cache*)))))

(define-stumpwm-type-with-completion :aliased-url
    (progn
      (statusor:error-to-signal (webdav-maybe-init))
      (statusor:error-to-signal (url-launcher-list-url-keys)))
  :key-fn file-namestring
  :value-fn
  (lambda (webdav-path)
    (statusor:error-to-signal
     (cons webdav-path (url-launcher-cat-webdav-path webdav-path)))))

(define-stumpwm-type-with-completion :url-launcher-key
    (progn
      (statusor:error-to-signal (webdav-maybe-init))
      (statusor:error-to-signal (url-launcher-list-url-keys)))
  :key-fn file-namestring)

(defcommand launch-url (key-url) ((:aliased-url "enter url key: "))
  "Do a completing read of stored keys, then launch url"
  (when key-url
    (let* ((url (expand-user (cdr key-url)))
           (opener (url-command url)))
      (if (functionp opener)
          (stumpwm::lparallel-future (funcall opener url))
          (start-porcess-with-logging opener (list url)))
      ;;log to different file? or at least add tags
      (log-timestamped-entry url *search-history-filename*))))

;; TODO rename key => alias, launcher-append-url => launcher-put
(defcommand launcher-append-url (key &optional url)
    ((:string "enter new key: ")
     (:string nil ))
  "Read a new key-url pair, defaulting to current (firefox) browser url"
  (setq url (or url (url-launcher-get-browser-current-url))
	key (trim-spaces key))
  (if (or (not (and key url))
	  (zerop (length key))
	  (zerop (length url))
	  (string= "NIL" key))
      (message "invalid key")
      (progn
        (unless *webdav-server-info* (webdav-load-server-info))
        (assert *webdav-server-info*)
        (statusor:error-to-signal
         (cladaver:put *webdav-server-info*
                       (merge-pathnames webdav-urls-prefix key)
                       url))
        (setf *url-values-cache*
              (remove-if
               (lambda (val)
                 (equal (pathname-name (car val)) key))
               *url-values-cache*))
	(echo (format nil "added: ~A" url)))))

(defcommand url-launcher-delete-url (webdav-path) ((:url-launcher-key "enter url key: "))
  "Do a completing read of stored keys, then delete the url"
  (cladaver:rm *webdav-server-info* webdav-path)
  (url-launcher-list-url-keys :skip-cache t))

(defun uri-decode (url)
  (reduce
   (lambda (string from-to)
     (ppcre:regex-replace-all (car from-to) string (cdr from-to)))
   '(("%25" "%")
     ("%20" " ")
     ("%2B" "[+]")
     ("%3a" ":")
     ("%2f" "/"))
   :initial-value url))

;; would still be nice to have an emacs-like
;; (interactive (list ...))
;; without having to defne custom, one-off types, e.g.
;; (defcommand search-engine-search (engine terms)
;;   ((:string "enter search engine to use: ")
;;   (:string "enter search terms: ")))
;; ...
;; )

(defun define-key-auto-from-commands-into-keymap ()
  ;;TODO
  ;;automatically find the best key for a set of named commands
  ;;for use the first character in the command name that hasn't been used
  )

(defun webdav-maybe-init ()
  (unless *webdav-server-info*
    (statusor:return-if-error (webdav-load-server-info) WEBDAV-MAYBE-INIT)
    ;; mkdir. may fail if already exists
    '(cladaver:mkdir *webdav-server-info* webdav-urls-prefix)))

(defun url-launcher-init ()
  (webdav-load-server-info)
  (ensure-directory-exists
   (uiop:pathname-parent-directory-pathname
    (uiop:ensure-directory-pathname *search-history-filename*))
   :max-parents 2)
  (dolist (class *browser-classes*)
    (pushnew `(:class ,class) stumpwm:*deny-raise-request*))
  (setf *suppress-deny-messages* t))
