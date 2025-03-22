(in-package :stumpwm)

;; things that used to be run by .xinitrc
;; TODO this file should be loadable as an sbcl script with no extra dependencies

(defun xmodmap-locate-file (&key
                              (hostname
                               (trim-spaces (run-shell-command "hostname" t)))
                              (xmodmap-dir #P"~/.xmodmap/"))
  (let* ((xmodmap-filename
           (loop for cand in (list hostname "default")
                 as pathname = (make-pathname
                                :name cand
                                :type "xmodmap"
                                :defaults
                                xmodmap-dir)
                 do (format t "xinitrc: value of pathname: ~A~%" pathname)
                   thereis (and (probe-file pathname)
                                pathname)))
         (host-specific-script (make-pathname :type "sh"
                                              :name hostname
                                              :defaults xmodmap-dir)))
    (values xmodmap-filename host-specific-script)))

(defun xmodmap-load ()
  (let ((xmodmap-pke "/tmp/xmodmap.pke"))
    (unless (probe-file xmodmap-pke)
      (run-shell-command (format nil "xmodmap -pke > ~A" xmodmap-pke) t)))

  (multiple-value-bind (xmodmap-filename host-specific-script)
      (xmodmap-locate-file)
    (assert xmodmap-filename)
    (when (probe-file host-specific-script)
      (run-shell-command (format nil "bash ~A" host-specific-script)))
    (loop for _ below 4
          as cmd = (format nil "xmodmap -verbose ~A" xmodmap-filename)
          do (run-shell-command cmd t)
          do (sleep .5))))

(defun run-startup-scripts ()
  (loop for script in (append
                       '(#P"~/.xsessionrc")
                       (directory #P"~/.stumpwmrc.d/bin/on-startup/*.*"))
        do (format t "running script ~A~%" script)
        do
           (run-shell-command (format nil "~A &" script) nil)))

(defvar *screensaver-proc* nil)
(defparameter *screensaver-lock-time-mins* 45)

(defun screen-lock-program ()
  (or (which "xsecurelock.sh")
      (which "xsecurelock")
      (which "xsecurelock.bak")))

(defun service-running-p (service-name)
  (let ((proc (sb-ext:run-program
               "sudo" `("service" ,service-name "status")
               :wait t :search t)))
    (zerop (slot-value proc 'SB-IMPL::%EXIT-CODE))))

(defun start-screensaver ()
  (let ((lock-program (screen-lock-program)))
    (unless (and lock-program
                 (which "xautolock"))
      (error "xsecurelock, xautolock not installed"))
    (unless (or
             (and *screensaver-proc*
                  (eq :RUNNING (slot-value *screensaver-proc* 'SB-IMPL::%STATUS)))
             (service-running-p "sedation"))
      (setf *screensaver-proc*
            ;; "xautolock -time 1 -locker xsecurelock"
            (SB-EXT:RUN-PROGRAM "xautolock"
                                (list "-time" (write-to-string *screensaver-lock-time-mins*)
                                      "-locker" lock-program)
                                :environment (cons "XSECURELOCK_WANT_FIRST_KEYPRESS=1"
                                                   (sb-ext:posix-environ))
                                :search t
                                :output t
                                :error t
                                :wait nil)))))

(defmacro with-elapsed-time (elapsed-time-ms-var form &body body)
  (let ((start-time-sym (gensym "start-time")))
    `(let ((,start-time-sym (get-internal-real-time)))
       ,form
       (let ((,elapsed-time-ms-var (/ (- (get-internal-real-time) ,start-time-sym) 1000)))
         ,@body))))

(unless (fboundp 'run-shell-command)
  (defun run-shell-command (cmd &optional sync)
    (declare (ignore sync))
    (with-output-to-string (s)
      (sb-ext:run-program "bash"
                          (list "-c" cmd)
                          :search t
                          :wait t
                          :output s)
      s)))

(unless (fboundp 'trim-spaces)
  (defun trim-spaces (str)
    (string-trim '(#\space #\tab #\newline) str)))

(unless (fboundp 'which)
  (defun which (program)
    (trim-spaces (run-shell-command
                  (format nil "which ~A" program)))))

(defun xinitrc-init ()
  (loop for i below 3 do
    (with-elapsed-time ms (xmodmap-load)
      (message "xmodmap load took ~,1fs" (/ ms 1000))))

  (with-elapsed-time ms (run-startup-scripts)
    (message "startup shell scripts took ~,1fs" (/ ms 1000)))

  (lparallel-future
   ;; allow the sedation service some time to start
   ;; before defaulting to xautolock
   (sleep 60)
   (start-screensaver))

  (run-shell-command "xsetroot -cursor_name left_ptr" t))
