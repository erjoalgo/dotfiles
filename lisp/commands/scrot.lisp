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


(defun pathname-to-url (pathname)
  (format nil "file://~A" pathname))

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
  ;; (unmap-message-window (current-screen))
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
            (when overwrite '("-o"))))
         proc
         output
         done-p)
    (unless box
      (case selection
        (:interactive
         (if (mouse-available-p)
             (push "-s" args)
             (setf box (grab-box))))
        (:fullscreen nil)
        (:window (push "-u" args))))
    (setf output
          (with-output-to-string (out-fh)
            (setf proc (SB-EXT:RUN-PROGRAM program
                                           args
                                           :search t
                                           :output out-fh
                                           :error out-fh
                                           :wait nil))
            (loop with start-time-secs = (GET-UNIVERSAL-TIME)
                  as done-p = (eq :EXITED (slot-value proc 'SB-IMPL::%STATUS)) ;; TODO
                  as elapsed-secs = (- (get-universal-time) start-time-secs)
                  as timeout-p = (> elapsed-secs timeout-secs)
                  while (not (or done-p timeout-p)) do
                    (sleep 1))))

    (cond
      ((zerop (slot-value proc 'SB-IMPL::%EXIT-CODE))
       ;; success
       t)
      ((not (zerop (slot-value proc 'SB-IMPL::%EXIT-CODE)))
       (error "scrot command non-zero exit: ~A~%~A"
              (slot-value proc 'SB-IMPL::%EXIT-CODE)
              output))

      ((not (eq :EXITED (slot-value proc 'SB-IMPL::%STATUS)))
       (error "scrot command timed out")))

    (when box
      (destructuring-bind ((x . y) (_right . _bot) (width . height)) box
        (assert (equal x (min x _right)))
        (assert (equal y (min y _bot)))
        (let* ((crop-spec (format nil "~Dx~D+~D+~D"
                                  width
                                  height
                                  x y))
               (cmd "mogrify")
               (args (list "-crop" crop-spec out-png)))
          (message "scrot: ~A ~{~A~^ ~}~%" cmd args)
          (run-command-sync-notify-on-error cmd args))))
    ;;why does this crash the session
    (when show
      (x-www-browser (pathname-to-url out-png) t))

    (set-x-selection out-png :clipboard)

    (when verbose
      (message "copied to cliboard: ~A" out-png))

    out-png-pathname))


(defun image-fn-to-text (image-fn)
  (let ((cmd (format nil "tesseract.sh ~A -" image-fn)))
    (trim-spaces (run-shell-command cmd t))))

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
    (set-x-selection ocr-text '(:interactive :primary :secondary :clipboard))
    (message "copied ocr of length ~D to clipboard..."
             (length ocr-text))
    '(spell-clipboard)))

(define-stumpwm-type :point (input prompt)
  (declare (ignore input))
  (read-one-line
   (current-screen)
   (concat prompt "(Return to continue): "))
  (multiple-value-bind (x y win)
      (xlib:global-pointer-position *display*)
    (declare (ignore win))
    (cons x y)))

(defun grab-pointer-position ()
  (multiple-value-bind (x y win)
      (xlib:global-pointer-position *display*)
    (declare (ignore win))
    (cons y x)))

(defun grab-pointer-prompt (prompt)
  (read-one-line (current-screen) prompt)
  (grab-pointer-position))

(defun grab-box (&optional prompt)
  "Returns a 3-tuple: (left top) (right bottom) (width height)"
  (let* ((prefix (if prompt (format nil "~A: " prompt) ""))
         (tl (grab-pointer-prompt
              (format nil "~Amove cursor to top-left corner of the box: " prefix)))
         (br (grab-pointer-prompt
              (format nil "~Amove cursor to bottom-right corner of the box: " prefix)))
         w h x y)
    (destructuring-bind (top . l) tl
      (destructuring-bind (b . r) br
        (setf w (- r l)
              h (- b top)
              x l
              y top)
        (assert (> w 0))
        (assert (> h 0))
        (list (cons x y) (cons r b) (cons w h))))))

(define-stumpwm-type :box (input prompt)
  (declare (ignore input))
  (grab-box prompt))

(defvar *byzanz-recording-control-port* 17909)

(defcommand byzanz-record (name duration box)
    ((:string " recording name: ")
     (:number "recording duration in seconds: ")
     (:box "byzanz bounding box: "))
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
                             *byzanz-recording-control-port*))))
         (box-args
           (when box
             (destructuring-bind ((x . y) _ (w . h)) box
               (list
                (format nil "--x=~D" x)
                (format nil "--y=~D" y)
                (format nil "--width=~D" w)
                (format nil "--height=~D" h))))))
    (set-x-selection (namestring recording-pathname) :clipboard)
    (message "starting byzanz recording in 1s...")
    (sleep 1)
    (unmap-all-message-windows)
    (run-command-async-notify
     "byzanz-record"
     `(,@duration-args ,@box-args ,recording-pathname)
     (lambda () (x-www-browser (pathname-to-url recording-pathname) t)))))

(defcommand byzanz-record-auto () ()
  (handler-case (byzanz-record-auto-stop)
    (USOCKET:CONNECTION-REFUSED-ERROR (_ex)
      (declare (ignore _ex))
      ;; no recording in progress. start a new one...
      (byzanz-record nil nil (grab-box "byzanz box: ")))))

(defcommand byzanz-record-auto-stop () ()
  (mozrepl:nc "localhost" *byzanz-recording-control-port* "1"))
