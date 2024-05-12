(in-package :STUMPWM)

(defparameter *stumpwm-top-directory*
  ;; TODO find current file, e.g. __file__
  ;; (UIOP/LISP-BUILD:CURRENT-LISP-FILE-PATHNAME)
  (merge-pathnames  ".stumpwmrc.d/lisp/" (user-homedir-pathname)))

(defparameter *data-private* #P"~/.local/stumpwm/")

(defparameter *data-private-one-way* #P"~/.local/stumpwm/")

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

(defparameter *browser-cmd*
  `("chrome.sh"))

(defparameter emacs-classes
  (list "emacs" "GoogleEmacs"))
