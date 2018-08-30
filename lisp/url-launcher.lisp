(defpackage #:url-launcher
  (:export
   #:launch-url
   #:launcher-append-url
   #:search-engine-search
   #:uri-encode
   )
  (:use #:tsv-db))

(defvar *search-history-fn*
  ;;todo should be in sensitive
  (merge-pathnames "search-history" (user-homedir-pathname)))

;;; Launcher
(defparameter *launcher-persistent-alist*
  ;;TODO sane path handling
  (make-persistent-alist
   :fn
   (make-pathname
    :directory (append (pathname-directory STUMPWM-TOP)
		       '("data" "url-launcher-urls"))
    :defaults (user-homedir-pathname))
   :fs-type :directory))

(persistent-alist-load-if-exists
 *launcher-persistent-alist*)

(push '*launcher-persistent-alist*
      *persistent-alist-syms*)

(defun url-launcher-get-browser-current-url ()
  (chrome-get-url))

(defun url-launcher-browser-new-tab (url)
  ;;(mozreplfirefoxnewtab url)
  ;; (echo (format nil "url is ~A" url))
  (SB-EXT:RUN-PROGRAM browser-name
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
    ("about:config" ,#'mozrepl-firefox-new-tab)))

(defun url-command (url)
  (loop for (regexp opener) in *url-command-rules*
     thereis (and (cl-ppcre:scan regexp url) opener)
     finally (return #'url-launcher-browser-new-tab)))

(define-stumpwm-type-for-completion-from-alist-key-only
  :launcher-url (persistent-alist-alist *launcher-persistent-alist*))

(defcommand launch-url (launcher-key) ((:launcher-url "enter url key: "))
  "do a completing read of stored keys, then launch url"
  (let ((url
	 (persistent-alist-get
			*launcher-persistent-alist* launcher-key)))
    ;(message "got url: ~A ~A" url launcher-key)
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
	(log-entry-timestamp url *search-history-fn*)))))

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
	(persistent-alist-push *launcher-persistent-alist*
			       key url)
	;;(setq *launcher-alist* (cons key url))
	(echo (format nil "added: ~A" url)))))

;;search-engine-search
(defparameter *search-engine-persistent-alist*
  (make-persistent-alist
   :fn (make-pathname
	:directory (append (pathname-directory STUMPWM-TOP)
			   '("data" "search-engines"))
	:defaults (user-homedir-pathname))
   :fs-type :single-tsv-file))

(persistent-alist-load-if-exists *search-engine-persistent-alist*)
(push '*search-engine-persistent-alist* *persistent-alist-syms*)

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

(define-stumpwm-type-for-completion-from-alist
  :search-engine (persistent-alist-alist *search-engine-persistent-alist*))

(defcommand search-engine-search
  (engine terms)
  ((:search-engine "search engine: ")
   (:string "query: "))

  "completing-read prompt for search engine if not provided. then use its format string to construct a url by uri-encoding search terms"

  (let (engine-fmt)
    (when (and engine terms)
      (if (not (setf engine-fmt
		     ;;ugly but this is to allow "search-engine-search ddg" command
		     (if (consp engine) (cadr engine)
		       (persistent-alist-get
			*search-engine-persistent-alist* engine))))
	  (message "no such engine: '~A'" engine)
	(let* (
	       (args (ppcre:regex-replace-all "\\n" (trim-spaces terms) " "))
	       (query (uri-encode args))
	       (url (format nil engine-fmt query)))
	  (url-launcher-browser-new-tab url)
	  (log-entry-timestamp (format nil "~A:~A" engine terms)
			       *search-history-fn*))))))

(defparameter DEFAULT-SEARCH-ENGINE "ddg")

(defcommand search-engine-search-clipboard () ()
  "search the clipboard contents"
  (search-engine-search DEFAULT-SEARCH-ENGINE (get-x-selection )))

(defcommand reload-search-engines () ()
  "reload search engines from file"
  (reload-persistent-alist "*SEARCH-ENGINE-PERSISTENT-ALIST*")
  (loop
     with used-letters = nil
     for (eng . fmt) in
       (reverse (persistent-alist-alist *SEARCH-ENGINE-PERSISTENT-ALIST*))
     as letter = (loop for letter across eng
		    unless (member letter used-letters :test 'eql)
		    return letter)
     as kbd = (kbd (format nil "~A" letter))
     do (if (not letter)
	    (warn "unable to find a letter for engine ~A" eng)
	    (progn
	      (format t "mapping ~A to ~A. used: ~A~%" eng letter
		      used-letters)
	      (define-key *search-engine-map* kbd
		(format nil "search-engine-search ~A" eng))
	      (push letter used-letters)))))

(defun define-key-auto-from-commands-into-keymap ()
  ;;TODO
  ;;automatically find the best key for a set of named commands
  ;;for use the first character in the command name that hasn't been used
  )



(dolist (class browser-classes)
  (push `(:class ,class) stumpwm:*deny-raise-request*))

(setf *suppress-deny-messages* t)
