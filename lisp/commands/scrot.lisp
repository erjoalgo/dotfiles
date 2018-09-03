(defparameter *scrots-top*
  (merge-pathnames  "pictures/auto-scrots/" (user-homedir-pathname)))

(defcommand scrot-cmd (name)
    ((:string "enter name for scrot: "))
  "take a non-full-screen screen shot. select box interactively"
  (take-scrot name :fullscreen-p nil))

(defparameter *scrot-date-format* "%e-%b-%Y-%H:%M:%S")

(defcommand scrot-cmd-anon () ()
  "take an anonymous non-full-screen screen shot. select box interactively"
  (take-scrot (time-format *scrot-date-format*)
              :eog-scrot nil))

(defcommand scrot-cmd-full-screen (name)
    ((:string "enter name for scrot: "))
  "take a full-screen screen shot"
  (take-scrot name :fullscreen-p t))

(defun take-scrot (name &key
                          (fullscreen-p nil)
                          (scrot-top *scrots-top*)
                          (verbose t)
                          (eog-scrot t)
                          (timeout-secs 15))
  "save a scrot to *scrots-top*"
  (ensure-directories-exist scrot-top)
  ;;TODO allow selecting region
  (unmap-message-window (current-screen))
  ;; (sleep 1)
  (let* ((out-png-pathname (merge-pathnames
                            (make-pathname :name name :type "png")
                            scrot-top))
         (out-png (namestring out-png-pathname)))

    (when (cl-ppcre:all-matches "\\s" out-png)
      (error "filename may not contain spaces: ~A" out-png))

    (let ((proc (SB-EXT:RUN-PROGRAM "shutter"
                                    `("-e" "-f" "-o" ,out-png "-n"
                                           ,@(unless fullscreen-p (list "-s")))
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

      ;;why does this crash the session
      (when eog-scrot
        (SB-EXT:RUN-PROGRAM "eog"
                            (list out-png)
                            :search t
                            :wait nil))

      (set-x-selection out-png :clipboard)

      (when verbose
        (message "copied to cliboard: ~A" out-png))

      out-png-pathname)))


(defun image-fn-to-text (image-fn)
  (let ((cmd (format nil "tesseract ~A -" image-fn)))
    (run-shell-command cmd t)))

(defcommand ocr-scrot-clipboard () ()
  "interactively prompt for a region of the screen, take a screenshot,
perform ocr on it, place ocr'd text into clipboard"
  (let* ((ocr-name "last-ocr")
         (ocr-png-filename (take-scrot ocr-name
                                       :fullscreen-p nil
                                       :verbose nil
                                       :eog-scrot nil))
         (ocr-text (image-fn-to-text ocr-png-filename)))
    (set-x-selection ocr-text :clipboard)
    (message "copied ocr of length ~D to clipboard..."
             (length ocr-text))))
