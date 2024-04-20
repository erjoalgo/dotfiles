(in-package :STUMPWM)

(defparameter *scrots-top*
  (merge-pathnames  "pictures/auto-scrots/" (user-homedir-pathname)))

(defcommand scrot-cmd (name)
    ((:string "enter name for scrot: "))
  "take a non-full-screen screen shot. select box interactively"
  (take-scrot name :selection :interactive))

(defparameter *scrot-date-format* "%d-%b-%Y-%H:%M:%S")

(defcommand scrot-cmd-anon () ()
  "take an anonymous non-full-screen screen shot. select box interactively"
  (take-scrot (time-format *scrot-date-format*)
              :show t))

(defcommand scrot-cmd-full-screen (name)
    ((:string "enter name for scrot: "))
  "take a full-screen screen shot"
  (take-scrot name :selection :fullscreen))

(defcommand scrot-cmd-full-screen-anon () ()
  "take a full-screen anonymous screen shot"
  (take-scrot (time-format *scrot-date-format*)
              :selection :fullscreen
              :show t))

(defcommand scrot-cmd-current-window-anon () ()
  "take an anonymous screen shot of the current window"
  (take-scrot (time-format *scrot-date-format*)
              :selection :window
              :show t))

(defun mouse-available-p ()
  (remove-if-not (lambda (pathname)
                   (ppcre:scan "mouse" (pathname-name pathname)))
                 (directory #P"/dev/input/*")))

(defvar *record-box-and-funcall-state*
  nil
  "Internal to record-box-and-funcall")

(defun take-scrot (name
                   &rest args
                   &key
                     (selection :interactive)
                     box
                     (scrot-top *scrots-top*)
                     (verbose t)
                     (show t)
                     (timeout-secs 20)
                     (overwrite nil))
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
         (args
           (append
            (list out-png "-z")
            (when overwrite '("-o"))
            (case selection
              (:interactive
               ;; (message "select a box in the screen...")
               ;; (sleep .5)
               ;; (unmap-all-message-windows)
               (if (mouse-available-p)
                   '("-s")
                   (progn
                     (echo (concat "scrot will resume after two "
                                   "clicks at the box edges..."))
                     (return-from take-scrot
                       (setf *record-box-and-funcall-state*
                             (list
                              (lambda (box)
                                (apply #'take-scrot name
                                       :selection box args))))))))
              (:fullscreen nil)
              (:window '("-u"))
              (t
               (setf box selection)
               nil))))
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
                 (error "scrot command failed:~%~A"
                        (if (not done-p) "timeout"
                            "non-zero exit status"))))

    (when box
      (destructuring-bind ((x1 . y1) (x2 . y2)) box
        (let* ((width (abs (- x2 x1)))
               (height (abs (- y2 y1)))
               (crop-spec (format nil "~Dx~D+~D+~D"
                                  width
                                  height
                                  (min x1 x2) (min y1 y2)))
               (cmd "mogrify")
               (args (list "-crop" crop-spec out-png)))
          (message "scrot: ~A ~{~A~^ ~}~%" cmd args)
          (run-command-sync-notify-on-error cmd args))))

    ;;why does this crash the session
    (when show
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
         (ocr-png-filename
           (take-scrot ocr-name
                       :selection :interactive
                       :verbose nil
                       :show nil
                       :overwrite t))
         (ocr-text (image-fn-to-text ocr-png-filename)))
    (set-x-selection ocr-text :clipboard)
    (message "copied ocr of length ~D to clipboard..."
             (length ocr-text))))

(define-stumpwm-type :point (input prompt)
  (declare (ignore input))
  (read-one-line
   (current-screen)
   (concat prompt "(Return to continue): "))
  (multiple-value-bind (x y win)
      (xlib:global-pointer-position *display*)
    (declare (ignore win))
    (cons x y)))

(defun record-box-and-funcall (&optional screen code x y)
  (declare (ignore screen code))
  (when *record-box-and-funcall-state*
    (let ((p (cons x y)))
      (message "~A" p)
      (case (length *record-box-and-funcall-state*)
        (1 (push p *record-box-and-funcall-state*)
         (echo "one more click..."))
        (2 (destructuring-bind (p1 fn) *record-box-and-funcall-state*
             (unmap-all-message-windows)
             (setf *record-box-and-funcall-state* nil)
             (sb-thread:make-thread
              (lambda (fn p1 p)
                (sleep .5)
                (funcall fn (list p1 p)))
              :arguments (list fn p1 p))))
        (t (error "record-box-and-funcall usage error: ~A"
                  *record-box-and-funcall-state*))))))


(defun grab-pointer-position ()
  (multiple-value-bind (x y win)
      (xlib:global-pointer-position *display*)
    (declare (ignore win))
    (cons x y)))


(add-hook *click-hook* 'record-box-and-funcall)

(defvar *byzanz-recording-control-port* 17909)

(defcommand byzanz-record (name duration)
    ((:string " recording name: ")
     (:number "recording duration in seconds: "))
  (assert (which "byzanz-record"))
  (let* ((name (or name
                   (time-format *scrot-date-format*)))
         (recording-pathname
           (merge-pathnames (make-pathname
                             :name name
                             :type "gif")
                            *scrots-top*))
         (duration-args
           (if duration (list "-d" duration)
               (list "-e"
                     (format nil "nc -l ~A ~D"
                             "-p" ;; not always the same
                             *byzanz-recording-control-port*)))))
    (set-x-selection (namestring recording-pathname) :clipboard)
    (message "starting byzanz recording in 1s...")
    (sleep 1)
    (unmap-all-message-windows)
    (run-command-async-notify
     "byzanz-record"
     `(,@duration-args ,recording-pathname))))

(defcommand byzanz-record-auto () ()
  (handler-case (byzanz-record-auto-stop)
    (USOCKET:CONNECTION-REFUSED-ERROR (_ex)
      (declare (ignore _ex))
      ;; no recording in progress. start a new one...
      (byzanz-record nil nil))))

(defcommand byzanz-record-auto-stop () ()
  (mozrepl:nc "localhost" *byzanz-recording-control-port* "1"))
