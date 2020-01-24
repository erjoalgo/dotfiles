(in-package :STUMPWM)

;; send USR1 to emacs process in current window to actiavte server

(defun window-class-matcher (window-classes)
  (format nil "(?i)^(~{~A~^|~})$" window-classes))

(defvar emacs-window-class-matcher
  (window-class-matcher emacs-classes))

(defun window-class-matches (window-class window-class-matcher)
  (cl-ppcre:scan window-class-matcher window-class))

(declaim (optimize (debug 3) (speed 0)))

(defun focus-frame-signal-current-emacs (&optional new-group old-group)
  (declare (ignore old-group))
  (let* ((windows (group-windows new-group))
         (emacs-wins
           (remove-if-not (lambda (win)
                            (window-class-matches (window-class win)
                                                  emacs-window-class-matcher))
                          windows)))
    (when emacs-wins
      (let* ((emacs-win (car emacs-wins))
             (pid (window-pid emacs-win)))
        (run-shell-command (format nil "kill -USR1 ~D" pid))))))

(add-hook STUMPWM:*focus-group-hook* 'focus-frame-signal-current-emacs)
