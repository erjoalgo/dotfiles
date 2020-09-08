(in-package :stumpwm)

(defcommand read-one-key (keys) ((:key-seq "Describe Key:"))
  "Either interactively type the key sequence or supply it as text. This
  command prints the command bound to the specified key sequence."
  keys)

(defcommand middle-click () ()
  ;; (echo "middle-click")
  ;; (send-fake-click (current-window) 2)
  (xdotool '("click" "2")))
