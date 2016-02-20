(defvar HOSTNAME (string-trim-whitespace (run-shell-command "hostname" t) ))
(defvar per-keyboard-xmodmap-path
  (stumpwm-merger (format nil "xmodmap/per-keyboard/~A" HOSTNAME)))

(when (probe-file per-keyboard-xmodmap-path)
  (run-shell-command (format nil "xmodmap ~A" per-keyboard-xmodmap-path)))
(load-xmodmap-rules HOSTNAME)
