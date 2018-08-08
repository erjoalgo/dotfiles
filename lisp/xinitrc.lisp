;; things that used to be run by .xinitrc

(defun xmodmap-load ()
  (let ((xmodmap-pke "/tmp/xmodmap.pke"))
    (unless (probe-file xmodmap-pke)
      (run-shell-command (format nil "xmodmap -pke > ~A" xmodmap-pke) t)))

  (let* ((hostname (trim-spaces (run-shell-command "hostname" t)))
         (xmodmap-dir #P"~/.xmodmap/")
         (xmodmap-filename
           (loop for cand in (list hostname "default")
                 as pathname = (merge-pathnames (pathname cand) xmodmap-dir)
                   thereis (and (probe-file pathname)
                                pathname)))
         )
    (assert xmodmap-filename)
    (loop for _ below 2 do
      (run-shell-command (format nil "xmodmap ~A" xmodmap-filename) t))))

(xmodmap-load)

(defun run-startup-scripts ()
  (loop for script in (append
                       '(#P"~/.xsession")
                       (directory #P"~/.stumpwmrc.d/scripts/on-startup/*.*"))
        do (format t "running script ~A~%" script)
        do
           (run-shell-command (format nil "~A &" script) nil)))

(run-startup-scripts)
