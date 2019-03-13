(defparameter *scrots-top*
  (merge-pathnames  "pictures/auto-scrots/" (user-homedir-pathname)))

(defcommand scrot-cmd (name)
    ((:string "enter name for scrot: "))
  "take a non-full-screen screen shot. select box interactively"
  (take-scrot name :selection :interactive))

(defparameter *scrot-date-format* "%e-%b-%Y-%H:%M:%S")

(defcommand scrot-cmd-anon () ()
  "take an anonymous non-full-screen screen shot. select box interactively"
  (take-scrot (time-format *scrot-date-format*)
              :display-scrot nil))

(defcommand scrot-cmd-full-screen (name)
    ((:string "enter name for scrot: "))
  "take a full-screen screen shot"
  (take-scrot name :selection :fullscreen))

(defcommand scrot-cmd-full-screen-anon () ()
  "take a full-screen anonymous screen shot"
  (take-scrot (time-format *scrot-date-format*)
              :selection :fullscreen
              :display-scrot nil))

(defun take-scrot (name &key
                          (selection :interactive)
                          box
                          (scrot-top *scrots-top*)
                          (verbose t)
                          (display-scrot t)
                          (timeout-secs 20))
  "save a scrot to *scrots-top*"
  (ensure-directories-exist scrot-top)
  ;;TODO allow selecting region
  (unmap-message-window (current-screen))
  ;; (sleep 1)
  (let* ((out-png-pathname (merge-pathnames
                            (make-pathname :name name :type "png")
                            scrot-top))
         (out-png (let ((out-png (namestring out-png-pathname)))
                    (if (cl-ppcre:all-matches "\\s" out-png)
                        (error "filename may not contain spaces: ~A" out-png)
                        out-png)))
         (program "scrot")
         (args (append (list out-png)
                       (case selection
                         (:interactive
                          (echo "select a box in the screen...")
                          (sleep .5)
                          (unmap-message-window (current-screen))
                          '("-s"))
                         (:fullscreen nil)
                         (:window '("-u"))
                         (t
                          (setf box selection)))))
         (proc (SB-EXT:RUN-PROGRAM program
                                   args
                                   :search t
                                   :output t
                                   :error t
                                   :wait nil)))

        (loop with start-time-secs = (GET-UNIVERSAL-TIME)
           as done-p = (eq :EXITED (slot-value proc 'SB-IMPL::%STATUS)) ;; TODO
           as elapsed-secs = (- (get-universal-time) start-time-secs)
           as timeout-p = (> elapsed-secs timeout-secs)
           while (not (or done-p timeout-p)) do
             (sleep 1)
           finally
             (if (not (and done-p
                           (zerop (slot-value proc 'SB-IMPL::%EXIT-CODE))))
                 (error "scrot command failed: ~A"
                        (if (not done-p) "timeout"
                            "non-zero exit status"))))

        (when box
          (destructuring-bind ((x1 y1) (x2 y2)) box
            ;; (apply #'append box)
            ;; convert "$imagePath" -crop ${width}x${height}+${left}+${top} "$imagePath"
            (let ((width (abs (- x2 x1)))
                  (height (abs (- y2 y1))))
              (list "mogrify" out-png "-crop"
                    (format nil "~Dx~D+~D-~D"
                            width
                            height
                            x1 y1)
                    out-png))))

        ;;why does this crash the session
        (when display-scrot
          (SB-EXT:RUN-PROGRAM "eog"
                              (list out-png)
                              :search t
                              :wait nil))

        (set-x-selection out-png :clipboard)

        (when verbose
          (message "copied to cliboard: ~A" out-png))

        out-png-pathname))


(defun image-fn-to-text (image-fn)
  (let ((cmd (format nil "tesseract ~A -" image-fn)))
    (run-shell-command cmd t)))

(defcommand ocr-scrot-clipboard () ()
  "interactively prompt for a region of the screen, take a screenshot,
perform ocr on it, place ocr'd text into clipboard"
  (let* ((ocr-name "last-ocr")
         (ocr-png-filename (take-scrot ocr-name
                                       :selection :interactive
                                       :verbose nil
                                       :display-scrot nil))
         (ocr-text (image-fn-to-text ocr-png-filename)))
    (set-x-selection ocr-text :clipboard)
    (message "copied ocr of length ~D to clipboard..."
             (length ocr-text))))
