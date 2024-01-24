(in-package :STUMPWM)

(defun start-apps-default ()
  (correct-screen-fix-display-prefs)
  (raise-browser)
  (raise-x-terminal-emulator)
  (emacs))

(defun startup-apps-run ()
  (let ((hostname-specific-apps-pathname
         (merge-pathnames (format nil ".~A-apps.lisp" (SB-UNIX:UNIX-GETHOSTNAME))
                          (user-homedir-pathname))))
    (if (probe-file hostname-specific-apps-pathname)
        (load hostname-specific-apps-pathname)
        (start-apps-default))))
