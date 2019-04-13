(defparameter BUTTONS-ROOT
  (merge-pathnames "button-imgs" DATA-PRIVATE))

(define-stumpwm-type-from-wild-pathname :button-pathname
    (merge-pathnames (make-pathname :type "png" :name :WILD) BUTTONS-ROOT)
  :allow-nonexistent t)

(ensure-directory-exists BUTTONS-ROOT :max-parents t)

(defcommand click-button (button-pathname) ((:button-pathname "enter button image: "))
  (message "button image is ~A" button-pathname))

(defcommand define-button  (button-pathname) ((:button-pathname "enter button image: "))
  (let* ((name (pathname-name button-pathname))
	 (parent-dir (make-pathname :name nil :type nil :defaults button-pathname)))

    (ensure-directory-exists BUTTONS-ROOT)
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
