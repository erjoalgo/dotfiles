(in-package :STUMPWM)

(defun run-shell-command-print (cmd &optional collect-output-p)
  (print cmd)
  (run-shell-command cmd collect-output-p))

(defstruct xrandr-display id state mode modes connected-p extra)
(defstruct xrandr-mode width height rates active preferred
  resolution-string)

(defun xrandr-parse-mode (mode-line)
  (ppcre:register-groups-bind
   ((#'parse-integer w) (#'parse-integer h) rest)
   ("^ *([0-9]+)x([0-9]+) *(.*)$" mode-line)
   (declare (ignore rest))
   (make-xrandr-mode
    :width w
    :height h
    :resolution-string (format nil "~Dx~D" w h)
    :rates (mapcar #'read-from-string
                   (ppcre:all-matches-as-strings "[0-9]+[.][0-9]+" mode-line))
    :preferred (not (null (ppcre:scan "[+]" mode-line)))
    :active (not (null (ppcre:scan "[*]" mode-line))))))

(defun xrandr-displays ()
  ;; return a list of xrandr-display
  (loop
    with lines = (cdr (ppcre:split #\Newline (run-shell-command "xrandr -q" t)))
    with pop-line = (lambda (&optional no-split-p) (if no-split-p (pop lines)
                                                       (ppcre:split " +" (pop lines))))
    while lines
    collect (destructuring-bind (id state . etc) (funcall pop-line)
              (declare (ignore etc))
              (let ((modes (loop
                             as line = (car lines)
                             while (ppcre:scan "^ +[0-9]+x[0-9]+" line)
                             collect (xrandr-parse-mode (funcall pop-line t))))
                    (extra (loop
                             while (ppcre:scan "^ +" (car lines))
                             collect (funcall pop-line))))
                (make-xrandr-display :id id :state state
                                     :modes modes
                                     :connected-p (equal state "connected")
                                     :extra extra
                                     :mode (loop for mode in modes thereis
                                                                   (and (xrandr-mode-active mode) mode)))))))

(defun xrandr-display-prefs (&key (prefs-file #P"~/.xdisplays"))
  (when (probe-file prefs-file)
    (with-open-file (fh prefs-file
                        :direction :input)
      (loop as line = (read-line fh nil nil)
            while line
            unless (equal "" line)
              collect (destructuring-bind (display mode)
                          (cl-ppcre:split #\Tab line)
                        (cons display (xrandr-parse-mode mode)))))))

(defun correct-screen (&optional order)
  (let* ((displays (xrandr-displays))
         (connected (remove-if-not 'xrandr-display-connected-p
                                   displays))
         (to-connect-ordered (if order (loop for ith in order
                                             collect (nth ith connected))
                                 connected))
         (to-disconnect (remove-if (lambda (display)
                                     (member display to-connect-ordered))
                                   displays)))
    ;; disconnect
    (mapc (lambda (off-display)
            (run-shell-command-print
             (format nil "xrandr --output ~A --off"
                     (xrandr-display-id off-display))))
          to-disconnect)
    ;; connect displays in order
    (loop for display in to-connect-ordered
          with pos-x = 0
          with display-mode-prefs = (xrandr-display-prefs)
          as mode = (or (cdr (assoc (xrandr-display-id display) display-mode-prefs :test #'equal))
                        (car (xrandr-display-modes display)))
          do (let* ((id (xrandr-display-id display))
                    (mode-string (xrandr-mode-resolution-string mode))
                    (pos-string (format nil "~Dx0" pos-x))
                    (cmd (format nil "xrandr --output ~A --mode ~A --pos ~A"
                                 id mode-string pos-string )))
               (run-shell-command-print cmd)
               (incf pos-x (xrandr-mode-width mode))))))

(defun correct-screen-fix-display-prefs ()
  (loop with fixes = nil
        with id-mode-prefs = (xrandr-display-prefs)
        for display in (xrandr-displays) do
          (with-slots (id mode state) display
            (when (equal state "connected")
              (let* ((id-mode-pref (assoc id id-mode-prefs :test #'equal))
                     (mode-pref (cdr id-mode-pref)))
                (when (and mode-pref (not (equal (xrandr-mode-resolution-string mode-pref)
                                                 (xrandr-mode-resolution-string (xrandr-display-mode display)))))
                  (push (format nil "--output ~A --mode ~A" id (xrandr-mode-resolution-string mode-pref))
                        fixes)))))
        finally (when fixes
                  (let ((cmd (format nil "xrandr ~{~A~^ ~}" fixes)))
                    (format t "running... ~A~%" fixes)
                    (run-shell-command cmd)))))

(defcommand correct-screen-prompt-display-order () ()
  "correct screen, prompting for display order"
  (let* ((displays (remove-if-not
                    'xrandr-display-connected-p
                    (xrandr-displays)))
         (prompt (format nil "~{~{~a: \"~a\"~}~^, ~}: "
                         (loop for display in (mapcar 'xrandr-display-id displays)
                               for i from 0
                               collect (list i display))))
                                        ;(line (read-one-line (current-screen) prompt))
                                        ;(line (progn (echo (mapcar 'caar info))
                                        ;(read-one-line (current-screen) ": ")))
         (line (if (null (cdr displays))
                   ;; `((0 . ,(car displays)))
                   "0"
                   (read-one-line (current-screen)
                                  prompt)))
         (order (loop for c across line
                      collect (- (char-code c) (char-code #\0))))
         )
    (when (and order
               (>= (apply 'max order) (length displays)))
      (error "index out of bounds"))
    (correct-screen order)))

(defcommand correct-screen-no-prompt
    () ()
    "correct screen without prompt"
    (let* ((order
            (loop for display in (remove-if-not
                                  'xrandr-display-connected-p
                                  (xrandr-displays))
               for i from 0
               collect i)))
      (message-wrapped "~D output~:P detected" (length order))
      (correct-screen order)))

(defcommand correct-screen-select-mode () ()
  "select a mode for current displays"
  (let* ((display (selcand:select :candidates
                                 (remove-if-not 'xrandr-display-connected-p
                                                (xrandr-displays))
                                 :prompt "select display: "
                                 :stringify-fn #'XRANDR-DISPLAY-ID))
         (mode-stringify
          (lambda (mode)
            (with-slots (width height active) mode
              (format nil "~A~Dx~D"
                      (if active "*" "")
                      width height))))
        (mode (selcand:select :candidates (XRANDR-DISPLAY-MODES display)
                              :prompt "select mode: "
                              :stringify-fn
                              (lambda (mode)
                                (with-slots (width height active) mode
                                  (format nil "~A~Dx~D"
                                          (if active "*" "")
                                          width height)))
                              :display-candidates t)))
    (run-command-async-notify "xrandr"
                              (list "--output" (XRANDR-DISPLAY-ID display)
                                    "--mode" (funcall mode-stringify mode)))))

(define-stumpwm-type-for-completion
    :xrandr-rot
    '("left" "right" "normal" "inverted"))

(defcommand rotate-screen (orientation)
    ((:xrandr-rot "enter xrandr orientation: "))
  "rotate screen"
  (let ((cmd (format nil
		     "xrandr --orientation ~A"
		     orientation)))
    (run-shell-command cmd t)
    cmd))

(defcommand invert-screen () ()
  "invert screen"
  (run-shell-command "xcalib -a -i"))
