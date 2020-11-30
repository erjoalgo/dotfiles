(in-package :STUMPWM)

(defparameter *stumpwm-top-directory*
  ;; TODO find current file, e.g. __file__
  ;; (UIOP/LISP-BUILD:CURRENT-LISP-FILE-PATHNAME)
  (merge-pathnames  ".stumpwmrc.d/lisp/" (user-homedir-pathname)))

(defparameter *data-private*
  (merge-pathnames "private-data/stumpwm/" (user-homedir-pathname)))

(defparameter *data-private-one-way*
  (merge-pathnames "private-data-one-way/stumpwm/" (user-homedir-pathname)))

(defparameter *data-dirs*
  (list
   (merge-pathnames
    "data/*/"
    (uiop:pathname-parent-directory-pathname *stumpwm-top-directory*))
   *data-private*
   *data-private-one-way*))

(defparameter *browser-classes*
  '("Iceweasel" "Firefox"  "Navigator"  "Chromium" "chromium-browser"
    "Tor Browser" "Google-chrome" "Firefox-esr"))

(defparameter *browser-name*
  (or
   (pathname-name
    (string-trim '(#\newline)
                 (run-shell-command "which chromium-browser chrome google-chrome chromium" t)))
   "chromium"))

(defparameter emacs-classes
  (list "emacs" "GoogleEmacs"))

(defparameter *machine-uuid* (machine-get-uuid))
