(defpackage #:tsv-db
  (:export
   #:make-persistent-alist
   #:persistent-alist-load
   #:persistent-alist-alist
   #:persistent-alist-get
   #:persistent-alist-fn
   #:completing-read-alist
   #:completing-read-alist-key-value
	)
  ;(:use #:util)
  )

(defstruct persistent-alist alist fn)

(defun tsv-to-alist (fn)
  (let ((contents (file-string fn))
	 ;;(regex (format nil "[~a~a]" (coerce '(#\Newline) 'string) (coerce '(#\Tab) 'string) ))
	 (regexp '(:sequence
		   :MULTI-LINE-MODE-P
		   :START-ANCHOR;^
		   (:GREEDY-REPETITION 0 nil (:INVERTED-CHAR-CLASS  #\Tab));[^\t]*
		   #\Tab;\t
		   (:GREEDY-REPETITION 0 nil (:INVERTED-CHAR-CLASS  #\Newline));[^\n]*
		   :END-ANCHOR));$
	 matches)
    (setf matches (ppcre::all-matches-as-strings  regexp contents))
    (reverse (loop for el in matches
		collect (cl-ppcre:split #\Tab el)))))

(defun tsv-add-entry (fn key value)
  (with-open-file (out fn
		       :if-does-not-exist :create
		       :if-exists :append
		       :direction :output)
    ;;(format out "~A	~A~%";;this is a tab
    (format out "~A~A~A~%"
	    key (coerce '(#\Tab) 'string) value)))

(defun persistent-alist-load (palist)
  (setf (persistent-alist-alist palist)
	(tsv-to-alist (persistent-alist-fn palist))))

(defun persistent-alist-push (palist key value)
  (push (cons key value) (persistent-alist-alist palist))
  (tsv-add-entry (persistent-alist-fn palist)
		 key value)) 

(defun persistent-alist-get (palist key)
  (cadr (assoc key
	       (persistent-alist-alist
		palist)
	       :test 'equal)))

(defun completing-read-alist (alist &key (prompt "enter key: "))
  (completing-read
   (current-screen)
   prompt
   ;;TODO coerce to strings
   (mapcar 'car alist)))

(defun completing-read-alist-key-value
    (alist
     &key
       (prompt "enter key: ")
       (space-trim t)
       (disallow-empty t)
       (verbose t))
  
  (let ((key (completing-read-alist alist :prompt prompt)))
    (when key
      (when space-trim (setf key (trim-spaces key)))

      (if (and disallow-empty (= (length key) 0))
	  (and verbose (echo "key must be nonempty")) nil)
	  (let ((key-value (assoc key alist  :test 'equal)))
	    (if (not key-value)
		(and verbose (message "no such key-value for key: '~a'" key) nil)
		key-value)))))

