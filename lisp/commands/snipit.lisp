(in-package :STUMPWM)

(defvar *snipit-upload-url*)

(defcommand take-scrot-snipit (&optional png) ()
  (assert *snipit-upload-url*)
  (let ((png (or png (pathname (get-x-selection nil :clipboard)))))
    (assert (probe-file png))
    (run-command-async
     "curl"
     (list "-sLF" (format nil "imagedata=@~A" png) *snipit-upload-url*)
     (retcode output)
     (let ((url output))
       (set-x-selection url :clipboard)
       (message-wrapped "^2created snipit: ~A^*" output)
       (run-command-retcode-output "curl" (list url))))))
