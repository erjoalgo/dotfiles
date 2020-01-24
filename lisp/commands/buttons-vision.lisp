(in-package :STUMPWM)

(defparameter *buttons-root*
  (merge-pathnames "button-imgs" *data-private*))

(define-stumpwm-type-from-wild-pathname :button-pathname
    (merge-pathnames (make-pathname :type "png" :name :WILD) *buttons-root*)
  :allow-nonexistent t)

(ensure-directory-exists *buttons-root* :max-parents t)

(defcommand click-button (button-pathname) ((:button-pathname "enter button image: "))
  (message "button image is ~A" button-pathname))

(defcommand define-button  (button-pathname) ((:button-pathname "enter button image: "))
  (let* ((name (pathname-name button-pathname))
	 (parent-dir (make-pathname :name nil :type nil :defaults button-pathname)))

    (ensure-directory-exists *buttons-root*)
    (ensure-directory-exists parent-dir)

    (setf button-pathname
          (take-scrot name
                      :selection :interactive
                      :scrot-top parent-dir
                      :verbose nil
                      :show nil))

    (unless (and button-pathname
                 (probe-file button-pathname))
      (error "scrot ~A was not created" button-pathname))))
