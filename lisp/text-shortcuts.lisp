(defun xdotool (cmd)
  (run-shell-command (format nil "xdotool ~A" cmd) t))

(defcommand type-string (string)
    ((:string "paste string to type: "))
  "type a given string. some browser forms disallow copy-pasting"
  (unmap-all-message-windows)
  (xdotool (concat "type " string)))

(defcommand type-clipbpard () ()
  "type the clipboard contents"
  (type-string (get-x-selection)))

(defvar *text-shortcuts* nil)

(define-stumpwm-type-for-completion :text-shortcut *text-shortcuts*)

(defcommand type-shortcut (shortcut)
    ((:text-shortcut "enter shortcut (tab completion): "))
  (type-string shortcut))

(defvar *text-shortcuts-map* (make-sparse-keymap))
(define-key *text-shortcuts-map* (kbd "H-t") "type-shortcut")

(defparameter TEXT-SHORTCUTS-PATHNAME
  (merge-pathnames "data/text-shortcuts" STUMPWM-TOP))

(defun text-shortcuts-load (&key (pathname TEXT-SHORTCUTS-PATHNAME))
  (setf *text-shortcuts*
        (when (probe-file pathname)
          (ppcre:split #\Newline (file-string pathname))))
  (message "loaded ~D text shortcuts" (length *text-shortcuts*)))

(defcommand text-shortcuts-add (shortcut &key (pathname TEXT-SHORTCUTS-PATHNAME))
    ((:string "enter text shortcut to add: "))
  (with-open-file (fh pathname
                      :direction :output
                      :if-exists :append
                      :if-does-not-exist :create)
    (format fh "~A~%" shortcut))
  (push shortcut *text-shortcuts*))

(text-shortcuts-load)
