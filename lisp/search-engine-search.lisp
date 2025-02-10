;; (CL:defpackage :SEARCH-ENGINE-SEARCH
;;   (:export
;;    #:search-engine-search
;;    #:*search-engine-map*
;;    #:define-search-engines
;;    #:search-engine-find-by-key))

;; (CL:in-package :SEARCH-ENGINE-SEARCH)

;; (CL:use-package :CL)

(in-package :STUMPWM)

(defvar *search-engines* nil "A list of search-engine structs.")

(defun define-search-engines (spec)
  "Parse and install a list search engines specified as a list of
  (KEY-BINDING ENGINE-ID URL-TEMPLATES) triples.

  URL-TEMPLATES should contain a list of at least one search engine URL
  with the query parameter replaced by the '~A' format
  specifier. This specifier will be expanded with the search query.

  If multiple engines are associated with a single spec, multiple tabs will
  be opened for the same search query.

  The first search engine in the list is used as the default
  if no explicit engine is specified."
  (loop
    for (key engine-id fmts) in spec
    as key-sanitized = (sanitize-key key)
    as engine = (make-search-engine :id engine-id :key key :url-templates fmts)
    as conflicting-engine = (search-engine-find-by-key key)
    do (if conflicting-engine
           (unless (equalp conflicting-engine engine)
             (warn
              "Skipping engine ~A bound to ~A which conflicts with existing: ~A"
              engine-id key conflicting-engine))
           (push engine *search-engines*)))
  (search-engine-install-keymap))

'(define-search-engines
  ;; provided as an example
  '(("g" "google" "https://www.google.com/search?num=20&q=~A")
    ("y" "youtube" "https://www.youtube.com/results?search_query=~A")
    ("m" "google-maps" "https://www.google.com/maps/search/~A")
    ("d" "ddg" "https://duckduckgo.com/lite/?q=~A")))

(defstruct search-engine id key url-templates)

(defparameter *default-engine*
  (make-search-engine :id "ddg"
                      :key "d"
                      :url-templates '("https://duckduckgo.com/lite/?q=~A")))

(defparameter *search-history-filename*
  (merge-pathnames "search-history" STUMPWM::*data-private-one-way*)
  "If not nil, search queries will be logged to this filename.")

(defvar *search-engine-map* (STUMPWM:make-sparse-keymap) "")

(defvar *search-engine-search-split-by-newline* t)

(STUMPWM:defcommand search-engine-search (&optional engine-id no-clipboard)
    ((:string "this prompt should never be used"))
  ;; We need to do interactive args ourselves to support displaying
  ;; search engine name in prompt. This is important to ensure the user
  ;; has selected the correct search engine from a key binding.
  "Use search ENGINE to execute search QUERY in a new browser tab or otherwise.

  Tab completion is available if engine is not provided."
  (declare (ignore no-clipboard))
  (let* ((engine-id
           (or engine-id
               (STUMPWM:completing-read (STUMPWM:current-screen) "Select search engine: "
                                        (mapcar #'search-engine-id *search-engines*)
                                        :require-match t)))
         (engine (or (search-engine-find-by-id engine-id)
                     (error "No engine found with id '~A': " engine-id)))
         (raw-query (STUMPWM:read-one-line
                     (STUMPWM:current-screen)
                     (format nil "~A query: ~%" engine-id)
                     :initial-input
                     ;; (unless no-clipboard (STUMPWM:get-x-selection))
                     ""))
         (query (ppcre:regex-replace-all "/" raw-query "%2F")))
    (when query
      (search-engine-search-noninteractive query engine))))

(defun search-engine-search-noninteractive (query &optional engine)
  (if *search-engine-search-split-by-newline*
      (lparallel-future
       (loop for query in (or (ppcre:split #\Newline query) '(""))
             do (search-engine-search-noninteractive-single query engine)
             do (sleep .5)))
      (search-engine-search-noninteractive-single query engine)))

(defun search-engine-search-noninteractive-single (query &optional engine)
  (assert query)
  (unless engine (setf engine *default-engine*))
  (let* ((query-sanitized (ppcre:regex-replace-all "\\n" (STUMPWM:trim-spaces query) " "))
	 (query-encoded (uri-encode query-sanitized)))
    (loop with tmpls = (search-engine-url-templates engine)
          for url-tmpl in (if (atom tmpls) (list tmpls) tmpls)
          as url = (format nil url-tmpl query-encoded)
          do (STUMPWM:x-www-browser url))
    (when *search-history-filename*
      (STUMPWM:log-timestamped-entry (format nil "~A:~A" engine query)
                                     *search-history-filename*))))

(defun search-engine-find-by-id (engine-id)
  (find engine-id *search-engines*
        :test (lambda (engine-id engine)
                (equal engine-id (search-engine-id engine)))))

(defun search-engine-find-by-key (engine-key)
  (find engine-key *search-engines*
        :test (lambda (engine-key engine)
                (equal engine-key (search-engine-key engine)))))

(defun search-engine-install-keymap ()
  "Install search engines into *search-engine-map*"
  (loop for engine in (reverse *search-engines*)
        do (STUMPWM:define-key *search-engine-map* (sanitize-key (search-engine-key engine))
	     (format nil "engsearch2 ~A" (search-engine-id engine)))))

;; make command name shorter to make help-map (?) more useful
(STUMPWM:defcommand-alias engsearch2 search-engine-search)

(defun uri-encode (search-query)
  (reduce
   (lambda (string from-to)
     (ppcre:regex-replace-all (car from-to) string (cdr from-to)))
   '(("%" "%25")
     (" " "%20")
     ("[+]" "%2B"))
   :initial-value search-query))

(defun sanitize-key (key)
  (if (STUMPWM::key-p key) key
      (STUMPWM:kbd (format nil "~A" key))))
