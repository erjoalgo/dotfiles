(setq XMODMAP-RULES-FN "per-keyboard-xmodmaps-rules.lisp")
(setq xmodmap-rules-verbose t)

(fset string-trim-whitespace (curry 'string-trim
				    '(#\Space #\Newline #\Backspace #\Tab 
				      #\Linefeed #\Page #\Return #\Rubout)))


(defun run-xmodmap-subcmd (subcmd)
  (let* ((cmd (format nil "xmodmap -e '~A'" subcmd)))
    (when xmodmap-rules-verbose
      (echo-format "running xmodmap command: ~A" cmd))
    (run-shell-command cmd)
    (print cmd)
    )
  
  )

(defun load-xmodmap-rules (key)
  (let* (
	 (rules (read-from-string (file-string (stumpwm-merger XMODMAP-RULES-FN))))
	 (matching (assoc key rules :test 'equal))
	 )
    (if matching
	(mapcar 'run-xmodmap-subcmd (cadr matching))
	(echo-format "no such entry for ~A" key)
	))
  )
  
(setq HOSTNAME (string-trim-whitespace (run-shell-command "hostname" t) ))
(load-xmodmap-rules HOSTNAME)
