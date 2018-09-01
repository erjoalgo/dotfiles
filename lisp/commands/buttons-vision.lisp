(defparameter BUTTONS-ROOT
  (merge-pathnames "private-data/button-imgs/" (user-homedir-pathname)))

(define-stumpwm-type-from-wild-pathname :button-pathname
    (merge-pathnames (make-pathname :type "png" :name :WILD) BUTTONS-ROOT)
  :allow-nonexistent t)

(ensure-directory-exists BUTTONS-ROOT :max-parents 1)

(defcommand click-button (button-pathname) ((:button-pathname "enter button image: "))
  (message "button image is ~A" button-pathname))

(defcommand define-button  (button-pathname) ((:button-pathname "enter button image: "))
  (let* ((name (pathname-name button-pathname))
	 (parent-dir (make-pathname :name nil :type nil :defaults button-pathname)))

    (ensure-directory-exists BUTTONS-ROOT)
    (ensure-directory-exists parent-dir)

    (setf button-pathname
          (take-scrot name
                      :fullscreen-p nil
                      :scrot-top parent-dir
                      :verbose nil
                      :eog-scrot nil))

    (unless (and button-pathname
                 (probe-file button-pathname))
      (error "scrot ~A was not created" button-pathname))))
