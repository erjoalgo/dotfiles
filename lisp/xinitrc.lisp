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

(defun run-startup-scripts ()
  (loop for script in (append
                       '(#P"~/.xsessionrc")
                       (directory #P"~/.stumpwmrc.d/scripts/on-startup/*.*"))
        do (format t "running script ~A~%" script)
        do
           (run-shell-command (format nil "~A &" script) nil)))

(defun xinitrc-was-run-p (&key (filename #P"/tmp/stumpwmrc.pid") write-p)
  (let ((pid (sb-posix:getpid)))
    (if (null write-p)
        (when (probe-file filename)
          (with-open-file (fh filename)
            (let* ((line (read-line fh))
                   (stored-pid (parse-integer line)))
              (= pid stored-pid))
            ))
        (with-open-file (fh filename :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
          (format fh "~D" pid)))))

(unless (xinitrc-was-run-p)
  (echo "running xinitrc commands...")
  (xmodmap-load)
  (run-startup-scripts)
  (xinitrc-was-run-p :write-p t))
