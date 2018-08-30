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

(defvar *screensaver-proc* nil)
(defparameter *screensaver-lock-time-mins* 15)

(defun start-screensaver ()
  (unless (and (which "xsecurelock")
               (which "xautolock"))
    (error "xsecurelock, xautolock not installed"))
  (unless (and *screensaver-proc*
               (eq :RUNNING (slot-value *screensaver-proc* 'SB-IMPL::%STATUS)))
    (setf *screensaver-proc*
          ;; "xautolock -time 1 -locker xsecurelock"
          (SB-EXT:RUN-PROGRAM "xautolock"
                              (list "-time" (write-to-string *screensaver-lock-time-mins*)
                                    "-locker" "xsecurelock")
                              :search t
                              :output t
                              :error t
                              :wait nil))))

(xmodmap-load)
(run-startup-scripts)
(start-screensaver)
