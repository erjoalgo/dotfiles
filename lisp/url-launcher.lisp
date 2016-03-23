(defpackage #:url-launcher
  (:export
   #:launch-url
   #:launcher-append-url
   #:search-engine-search
   #:uri-encode
   ;#:make-persistent-alist
   )
  (:use #:tsv-db))


(defparameter *launcher-persistent-alist*
  ;;TODO sane path handling
  (make-persistent-alist :fn
			 (stumpwm-merger "sensitive/url-launcher-data")))

(persistent-alist-load *launcher-persistent-alist*)

(defvar *search-engine-persistent-alist*
  (make-persistent-alist :fn (stumpwm-merger "sensitive/search-engines")))

(persistent-alist-load *search-engine-persistent-alist*)

(defvar *search-history-fn*
  ;;todo should be in sensitive
  (concat (sb-posix:getenv "HOME") "/" "search-history"))

(defparameter *url-command-rules*
  '(
    (".*[.]pdf$" "zathura")
    ;;(".*[.]pdf" "evince")
    ;;(".*[.]pdf" "gv")
    ("(^https?://.*|.*[.]html.*).*" mozrepl-firefox-new-tab)
    (".*[.](docx?|odt)$" "libreoffice")
    ("about:config"  mozrepl-firefox-new-tab)))

(defun url-command (url)
  (loop for (regexp opener) in *url-command-rules*
     thereis (and (cl-ppcre:scan regexp url) opener)))

(defcommand launch-url () ()
  "do a completing read of stored keys, then launch url"
  (let ((key-url (completing-read-alist-key-value
	      (persistent-alist-alist *launcher-persistent-alist*)
	      :prompt "enter url key: " )))
    (when key-url
      (let* ((url (expand-user (cadr key-url)))
	     (opener (url-command url)))
	(if (symbolp opener)
	    (funcall opener url)

	    (progn (SB-EXT:RUN-PROGRAM opener (list url)
				;;TODO output to tmp?
				:search t
				:wait nil 
				:output t
				:error t
				:input t)
		   nil ))))))

(defcommand launcher-append-url (key &optional url)
    ((:string "enter new key: ")
     (:string nil ))
  "read a new key-url pair, defaulting to current (firefox) browser url"

  (setq url (or url (mozrepl-firefox-get-url))
	key (trim-spaces key))
  (if (not (and key url (> (length key) 0) (> (length url) 0)))
      (echo (format nil 
		    "something failed: (and key url (> (length key) 0) (> (length url) 0)) ~a ~a ~a ~a"
		    (not (not key))
		    (not (not url))
		    (> (length key) 0)
		    (> (length url) 0)
		    ))
      (progn
	(tsv-add-entry (persistent-alist-fn
			*launcher-persistent-alist*)
		       key (escape-bash-single-quotes url))
	;;(setq *launcher-alist* (cons key url))
	(echo (format nil "added: ~A" url)))))


(defun uri-encode (search-terms)
  (reduce
   (lambda (string from-to)
     (ppcre:regex-replace-all (car from-to) string (cdr from-to)))
   '(("%" "%25")
     (" " "%20")
     ("[+]" "%2B"))
   :initial-value search-terms))

;;would be nice to have an emacs-like
;;(interactive (list ...))
;(defcommand search-engine-search (engine terms)
    ;((:string "enter search engine to use: ")
     ;(:string "enter search terms: "))

(defcommand search-engine-search
    (&optional engine terms)
    ()
    "completing-read prompt for search engine if not provided. then use its format string to construct a url by uri-encoding search terms"
  (let ((engine (or engine
		    (completing-read-alist
		     (persistent-alist-alist *search-engine-persistent-alist*)
		     :prompt "enter search engine: " )))
	(terms (or terms (read-one-line (current-screen) "enter search query: ")))
	engine-fmt)
    
    (when (and engine terms)
      (if
       (not (setf engine-fmt
		  (persistent-alist-get
		   *search-engine-persistent-alist* engine)))
	  (message "no such engine: ~A" engine)
	  
	  (let* (
		 ;;(args (escape-bash-single-quotes ))
		 (args terms)
		 (query (uri-encode args))
		 (url (format nil engine-fmt query)))
	    (mozrepl-firefox-new-tab url)
	    (log-entry-timestamp terms *search-history-fn*))))))




