(in-package :STUMPWM)

(defpackage #:url-launcher
  (:export
   #:launch-url
   #:launcher-append-url
   #:search-engine-search
   #:search-engines-reload
   #:uri-encode))

;; (use-package :statusor)

(defparameter *search-history-fn*
  (merge-pathnames "search-history" *data-private-one-way*))

(defvar *webdav-server-base-url* nil)

(defvar *webdav-server-info* nil)

(defun load-webdav-server-info ()
  (statusor:if-let-ok (err (format t "unable to load webdav server: ~A" err))
      ((url (statusor:nil-to-error *webdav-server-base-url*))
       ;; TODO extract actual hostname
       (auth (statusor:nil-to-error (authinfo:get-by :app "webdav")))
       (user (authinfo:alist-get-or-error :login auth))
       (password (authinfo:alist-get-or-error :password auth))
       ;; TODO auth
       )
    (setf *webdav-server-info*
          (cladaver:make-server-info :base-url url
                                     :username user
                                     :password password))))

(load-webdav-server-info)

;; note the trailing slash.
;; needed to allow merging additional pathname components
(defvar webdav-urls-prefix #P"/urls/")

(defun url-launcher-get-browser-current-url ()
  (mozrepl:chrome-get-url))

(defun url-launcher-browser-new-tab (url)
  (SB-EXT:RUN-PROGRAM *browser-name*
		      (list url)
		      :search t
		      :wait nil
		      :output t
		      :error t))

;; actually load from the file
(defparameter *url-command-rules*
  `(
    (".*[.]pdf$" "zathura")
    ("(^https?://.*|.*[.]html.*).*" ,#'url-launcher-browser-new-tab)
    (".*[.](docx?|odt)$" "libreoffice")
    ("about:config" ,#'mozrepl:firefox-new-tab)))

(defun url-command (url)
  (loop for (regexp opener) in *url-command-rules*
     thereis (and (cl-ppcre:scan regexp url) opener)
     finally (return #'url-launcher-browser-new-tab)))

(define-stumpwm-type-with-completion :aliased-url
    (cladaver:ls *webdav-server-info* webdav-urls-prefix)
  :key-fn file-namestring
  :value-fn
  (lambda (webdav-path)
    (statusor:error-to-signal
     (cladaver:cat *webdav-server-info* webdav-path))))

(defcommand launch-url (url) ((:aliased-url "enter url key: "))
  "Do a completing read of stored keys, then launch url"
  (when url
    (let* ((url (expand-user url))
           (opener (url-command url)))

      (if (functionp opener)
          (funcall opener url)
          (progn
            (run-shell-command (format nil "~A ~A" opener url))
            ;;TODO why this causes hang
            '(SB-EXT:RUN-PROGRAM opener  (list url)
              ;;TODO output to tmp?
              :search t
              :wait nil
              :output t
              :error t
              :input t)
            nil ))
      ;;log to different file? or at least add tags
      (log-entry-timestamped url *search-history-fn*))))

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
        (statusor:error-to-signal
         (cladaver:put *webdav-server-info*
                       (merge-pathnames webdav-urls-prefix key)
                       url))
	(echo (format nil "added: ~A" url)))))

;;search-engine-search
(defparameter *search-engine-persistent-alist*
  (make-instance 'psym-tsv
   :pathnames (loop for data-dir in *data-dirs*
                 collect (merge-pathnames "search-engines" data-dir))
   :short-description "search engines"))

(defun uri-encode (search-terms)
  (reduce
   (lambda (string from-to)
     (ppcre:regex-replace-all (car from-to) string (cdr from-to)))
   '(("%" "%25")
     (" " "%20")
     ("[+]" "%2B"))
   :initial-value search-terms))

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

;;would still be nice to have an emacs-like
;;(interactive (list ...))
;;without having to defne custom, one-off types
;(defcommand search-engine-search (engine terms)
    ;((:string "enter search engine to use: ")
     ;(:string "enter search terms: "))

(define-stumpwm-type-with-completion :search-engine
    (psym-records *search-engine-persistent-alist*)
  :key-fn car
  :value-fn cdr
  :no-hints nil)

(defcommand search-engine-search
    (engine terms)
    ((:search-engine "search engine: ")
     (:string "query: "))

  "completing-read prompt for search engine if not provided. then use its format string to construct a url by uri-encoding search terms"

  (when (and engine terms)
    (let ((engine-fmt
            (if (consp engine)
                (cadr engine)
                (cdr (alist-get engine (psym-records *search-engine-persistent-alist*))))))
      (if (not engine-fmt)
	  (error "no such engine: '~A'" engine)
	  (let* (
	         (args (ppcre:regex-replace-all "\\n" (trim-spaces terms) " "))
	         (query (uri-encode args))
	         (url (format nil engine-fmt query)))
	    (url-launcher-browser-new-tab url)
	    (log-entry-timestamped (format nil "~A:~A" engine terms)
			         *search-history-fn*))))))

(defparameter *default-search-engine* "ddg")

(defcommand search-engine-search-clipboard () ()
  "search the clipboard contents"
  (search-engine-search *default-search-engine* (get-x-selection )))

;; make command name shorter to make help-map (?) more useful
(defcommand-alias engsearch search-engine-search)

(defvar *search-engine-map* (make-sparse-keymap) "")

(defcommand search-engines-reload () ()
  "reload search engines from file"
  (psym-load *search-engine-persistent-alist*)
  (loop
    with used-letters = nil
    for (eng . fmt) in (psym-records *search-engine-persistent-alist*)
    as letter = (loop for letter across eng
		      unless (member letter used-letters :test 'eql)
		        return letter)
    do (if (not letter)
           (warn "unable to find a letter for engine ~A" eng)
           (progn
	     (define-key *search-engine-map* (kbd (format nil "~A" letter))
	       (format nil "engsearch ~A" eng))
	     (push letter used-letters)))))

(defun define-key-auto-from-commands-into-keymap ()
  ;;TODO
  ;;automatically find the best key for a set of named commands
  ;;for use the first character in the command name that hasn't been used
  )


(dolist (class *browser-classes*)
  (push `(:class ,class) stumpwm:*deny-raise-request*))

(defun url-launcher-init ()
  (ensure-directory-exists
   (uiop:pathname-parent-directory-pathname
    (uiop:ensure-directory-pathname *search-history-fn*))
   :max-parents 2)
  (make-instance 'psym-lines-list
   :pathnames (loop for data-dir in *data-dirs*
                 collect (merge-pathnames "url-launcher-urls/" data-dir))
   :short-description "launcher urls")
  (psym-load *launcher-persistent-alist*)
  (psym-load *search-engine-persistent-alist*)
  (setf *suppress-deny-messages* t))
