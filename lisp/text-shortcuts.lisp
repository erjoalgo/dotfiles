(defun xdotool (cmd)
  (run-shell-command (format nil "xdotool ~A" cmd) t))

(defcommand type-string (string)
    ((:string "paste string to type: "))
  "type a given string. some browser forms disallow copy-pasting"
  (unmap-all-message-windows)
  (xdotool (concat "type " string)))

(defcommand type-clipbpard () ()
  "type the clipboard contents"
  (type-string (get-x-selection))
  (message "done"))

(defvar *text-shortcuts* nil)

(define-stumpwm-type-for-completion :text-shortcut *text-shortcuts*)

(defcommand type-shortcut (shortcut)
    ((:text-shortcut "enter shortcut (tab completion): "))
  (type-string shortcut))

(defvar *text-shortcuts-map* (make-sparse-keymap))
(define-key *text-shortcuts-map* (kbd "H-t") "type-shortcut")
(define-key *text-shortcuts-map* (kbd "H-T") "text-shortcut-add")

(defparameter *text-shortcuts-alist*
  (make-psym
   :pathnames (loop for data-dir in DATA-DIRS
                 collect (merge-pathnames "text-shortcuts" data-dir))
   :driver psym-lines-list-driver
   :short-description "text shortcuts"))

(psym-load *text-shortcuts-alist*)

(define-stumpwm-type-with-completion
    :text-shortcut (alist (psym-records *text-shortcuts-alist*)))

(define-stumpwm-type-pathname
    :text-shortcut-pathname
  (psym-concrete-pathnames *text-shortcuts-alist* :include-nonexistent t))

(defcommand text-shortcut-add (shortcut pathname)
    ((:string "enter text shortcut to add: ")
     (:text-shortcut-pathname "select classification: "))
  (psym-add *text-shortcuts-alist* shortcut pathname))

