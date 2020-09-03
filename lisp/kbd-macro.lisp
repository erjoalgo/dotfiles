(in-package :stumpwm)

(defvar *last-kbd-macro* nil
  "A symbol denoting the last kbd macro command executed")

(defvar *kbd-macros* nil
  "A list of available kbd macros.")

(defcommand run-last-kbd-macro () ()
  (if (null *last-kbd-macro*)
      (progn
        (message "no known last kbd macro")
        (run-kbd-macro))
      (call-interactively (symbol-name *last-kbd-macro*))))

(defcommand run-kbd-macro (&optional kbd-macro-name) ()
  (unless kbd-macro-name
    (setf kbd-macro-name
          (selcand:select
           :candidates *kbd-macros*
           :prompt "select kbd macro: "
           :display-candidates t)))
  (setf *last-kbd-macro* kbd-macro-name)
  (call-interactively kbd-macro-name))

;; (defmacro def-kbd-macro (name args interactive docstring
;;                          &body body)
;;   `(progn
;;      (defcommand ,name ,args ,interactive
;;        ,docstring
;;        (setf *last-kbd-macro* ',name)
;;        ,@body)
;;      (pushnew ',name *kbd-macros*)))


(defmacro def-kbd-macro (name &rest rest)
  `(progn
     (defcommand ,name ,@rest)
     (pushnew ',name *kbd-macros*)))

(def-kbd-macro perf-render-link () () ""
  (run-command-sync-notify-on-error
   "xdotool" (list
              "key" "Left" ;; move to end of word
              "key" "Control+Right" ;; move to end of word
              ;; "key" "Control+Right" ;; move to end of word
              ;; "key" "Control+Right" ;; move to end of word
              "key" "Control+Shift+Left"
              "key" "Control+Shift+Left"
              "key" "Control+Shift+Left";; copy 3 word boundaries, e.g. cl/1234
              "key" "Control+c" ;; copy
              "key" "Control+k" ;; add link
              "key" "Tab" ;; focus on link value field
              "key" "Control+v" ;; paste
              "key" "Return"
              ;; "key" "Right"
              )))

(def-kbd-macro chrome-add-to-dictionary () () ""
  (unmap-all-message-windows)
  (run-command-sync-notify-on-error
   "xdotool" (list
              "click" "3"
              ;; "key" "a" ;; "add" to dictionary
              ))
  (redisplay)
  (run-command-sync-notify-on-error
   "xdotool" (list
              "sleep" "1" ;; "add" to dictionary
              "key" "a" ;; "add" to dictionary
              )))
