(in-package :STUMPWM)

(defpackage #:url-launcher
  (:export
   #:launch-url
   #:launcher-append-url
   #:search-engine-search
   #:search-engines-reload
   #:uri-encode))

(defparameter *search-history-fn*
  (merge-pathnames "search-history" *data-private-one-way*))

(defparameter *launcher-persistent-alist*
  (make-psym
   :pathnames (loop for data-dir in *DATA-DIRS*
                 collect (merge-pathnames "url-launcher-urls/" data-dir))
   :driver psym-dir-alist-driver
   :short-description "launcher urls"))

(defun url-launcher-get-browser-current-url ()
  (mozrepl:chrome-get-url))

(defun url-launcher-browser-new-tab (url)
  ;;(mozreplfirefoxnewtab url)
  ;; (echo (format nil "url is ~A" url))
  (SB-EXT:RUN-PROGRAM *browser-name*
		      (list url)
		      :search t
		      :wait nil
		      :output t
		      :error t))

;;actually load from the file
(defparameter *url-command-rules*
  `(
    (".*[.]pdf$" "zathura")
    ;;(".*[.]pdf" "evince")
    ;;(".*[.]pdf" "gv")
    ("(^https?://.*|.*[.]html.*).*" ,#'url-launcher-browser-new-tab)
    (".*[.](docx?|odt)$" "libreoffice")
    ("about:config" ,#'mozrepl:firefox-new-tab)))

(defun url-command (url)
  (loop for (regexp opener) in *url-command-rules*
     thereis (and (cl-ppcre:scan regexp url) opener)
     finally (return #'url-launcher-browser-new-tab)))

(define-stumpwm-type-with-completion
    :launcher-url (alist (psym-records *launcher-persistent-alist*))
  :sel-form (key (alist-get key alist)))

(defcommand launch-url (launcher-key-value) ((:launcher-url "enter url key: "))
  "do a completing read of stored keys, then launch url"
  (destructuring-bind (key . url) launcher-key-value
    (declare (ignore key))
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
	(log-entry-timestamped url *search-history-fn*)))))

(defcommand launcher-append-url (key &optional url)
    ((:string "enter new key: ")
     (:string nil ))
  "read a new key-url pair, defaulting to current (firefox) browser url"

  (setq url (or url (url-launcher-get-browser-current-url))
	key (trim-spaces key))
  (if (or (not (and key url))
	  (zerop (length key))
	  (zerop (length url))
	  (string= "NIL" key))
      (message "invalid key")
      (progn
        (psym-add *launcher-persistent-alist* (cons key url)
                  ;; TODO select public or private...
                  )
	;;(setq *launcher-alist* (cons key url))
	(echo (format nil "added: ~A" url)))))

;;search-engine-search
(defparameter *search-engine-persistent-alist*
  (make-psym
   :pathnames (loop for data-dir in *data-dirs*
                 collect (merge-pathnames "search-engines" data-dir))
   :driver psym-tsv-alist-driver
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

(define-stumpwm-type-with-completion
    :search-engine (alist (psym-records *search-engine-persistent-alist*))
  :sel-form (key (alist-get key alist)))

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
  (make-psym
   :pathnames (loop for data-dir in *data-dirs*
                 collect (merge-pathnames "url-launcher-urls/" data-dir))
   :driver psym-dir-alist-driver
   :short-description "launcher urls")
  (psym-load *launcher-persistent-alist*)
  (psym-load *search-engine-persistent-alist*)
  (setf *suppress-deny-messages* t))
