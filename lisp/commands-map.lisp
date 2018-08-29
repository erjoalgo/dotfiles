(defcommand search-engine-search-clipboard () ()
  "search the clipboard contents"
    (search-engine-search "ddg" (get-x-selection )))

(defun xdotool (cmd)
  (run-shell-command (format nil "xdotool ~A" cmd) t))

(defcommand type-string (s)
    ((:string ))
  "type a given string"
  (xdotool (concat "type " s)))

;;(setq *commands-map* (make-sparse-keymap))
(defvar *snippets-map*
  (make-sparse-keymap))

(define-key-bindings
  *commands-map*
  '(
   ("g" "search-engine-search-clipboard")
   ("i" *snippets-map*)
   ("y" "youtube-wget")))

;;TODO identify pressed keys and release them
(define-key-bindings
  *snippets-map*
  '(("@" "type-string erjoalgo@gmail.com")))

(defmacro defcommand-xdotool-type (text-form &key (sleep-time .5) name)
  (setf name (or name text-form))
  `(defcommand ,(intern (string-upcase (format nil "xdotool-type-~A" name)))
       () ()
     ,(format nil "autogen cmd to type '~A'" name)
     (run-shell-command
      ,(format nil "sleep ~D && xdotool type '~A'" sleep-time
	       text-form))))

(defcommand-xdotool-type "ernesto.alfonsogonzalez@ge.com")
(defcommand-xdotool-type "erjoalgo@gmail.com")
(defcommand-xdotool-type (get-x-selection) :name "clipboard")
