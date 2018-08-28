(defun start-apps-default ()
  (correct-screen-fix-display-prefs)
  (raise-window-browser)
  (raise-window-x-terminal-emulator)
  (emacs))

(let ((hostname-specific-apps-pathname
        (merge-pathnames (format nil ".~A-apps.lisp" (SB-UNIX:UNIX-GETHOSTNAME))
                         (user-homedir-pathname))))
  (if (probe-file hostname-specific-apps-pathname)
      (load hostname-specific-apps-pathname)
      (start-apps-default)))
