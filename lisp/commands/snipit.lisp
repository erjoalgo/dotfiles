(in-package :STUMPWM)

(defvar *snipit-upload-url*)

(defcommand take-scrot-snipit (&optional png) ()
  "take screenshot using the snipit cli"
  (run-command-async
   "snipit" ()
   (lambda (retcode output)
     (ppcre:register-groups-bind (url)
         ("Uploaded to: (http.*)" output)
       (set-x-selection url :clipboard)
       (message-wrapped "^2created snipit at: ~A^*" url)))))
