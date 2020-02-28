(in-package :STUMPWM)

(defun xdotool (cmds)
  (sb-ext:run-program "xdotool" cmds
                      :wait nil
                      :search t))

(defcommand type-string (string &key delay-millis)
    ((:string "paste string to type: "))
  "type a given string. some browser forms disallow copy-pasting"
  (unmap-all-message-windows)
  (xdotool
   `("type"
       ,@(when delay-millis `(,(format nil "--delay ~D " delay-millis)))
       ,string)))

(defcommand type-clipbpard () ()
  "type the clipboard contents"
  (type-string (get-x-selection))
  (message "done"))

(defcommand type-clipbpard-slow () ()
  "type a given string, slowly"
  (type-string (get-x-selection) :delay-millis 1000))

(defvar *text-shortcuts* nil)

(define-stumpwm-type-for-completion :text-shortcut *text-shortcuts*)

(defcommand type-shortcut (shortcut)
    ((:text-shortcut "enter shortcut (tab completion): "))
  (type-string shortcut))

(defvar *text-shortcuts-map* (make-sparse-keymap))
(define-key *text-shortcuts-map* (kbd "H-t") "type-shortcut")
(define-key *text-shortcuts-map* (kbd "H-T") "text-shortcut-add")

(defparameter *text-shortcuts-alist*
  (make-instance 'psym-lines-list
   :pathnames (loop for data-dir in *data-dirs*
                 collect (merge-pathnames "text-shortcuts" data-dir))
   :short-description "text shortcuts"))

(define-stumpwm-type-with-completion
    :text-shortcut
    (psym-records *text-shortcuts-alist*)
  :no-hints nil)

(define-stumpwm-type-pathname
    :text-shortcut-pathname
  (psym-non-wild-pathnames *text-shortcuts-alist* :include-nonexistent t))

(defcommand text-shortcut-add (shortcut pathname)
    ((:string "enter text shortcut to add: ")
     (:text-shortcut-pathname "select classification: "))
  (psym-add *text-shortcuts-alist* shortcut pathname))

(defun text-shortcuts-init ()
  (psym-load *text-shortcuts-alist*))
