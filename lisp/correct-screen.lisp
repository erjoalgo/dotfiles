(in-package :STUMPWM)

(defun run-shell-command-print (cmd &optional collect-output-p)
  (print cmd)
  (run-shell-command cmd collect-output-p))

(defstruct xrandr-display id state mode modes connected-p
  current-mode primary-p features inverted-p raw-line)

(defstruct xrandr-mode width height rates active preferred
  resolution-string ratio)

(defstruct xrandr-pref display-mode order)

(defun xrandr-parse-mode (mode-line)
  (ppcre:register-groups-bind
      ((#'parse-integer w) (#'parse-integer h) qualifier rest)
      ("^ *([0-9]+)x([0-9]+)([iR])? *(.*)$" mode-line)
    (declare (ignore rest qualifier))
    (make-xrandr-mode
     :width w
     :height h
     :resolution-string (format nil "~Dx~D" w h)
     :rates (mapcar #'read-from-string
                    (ppcre:all-matches-as-strings
                     "[0-9]+[.][0-9]+"
                     mode-line))
     :preferred (not (null (ppcre:scan "[+]" mode-line)))
     :active (not (null (ppcre:scan "[*]" mode-line)))
     :ratio (/ w h))))

(defun xrandr-displays ()
  ;; return a list of xrandr-display
  (loop
    with lines = (cdr (ppcre:split #\Newline (run-shell-command "xrandr -q" t)))
    with pop-line = (lambda (&optional no-split-p) (if no-split-p (pop lines)
                                                       (ppcre:split " +" (pop lines))))
    while lines
    as line = (funcall pop-line t)
    as display
      = (ppcre:register-groups-bind
            (id state etc features-string)
            ("([^ ]+) ([^ ]+) ([^(]+ )?([(].*?[)])"
             line)
          (let ((current-mode
                  (ppcre:scan-to-strings "[0-9]+x[0-9]+[+][0-9]+[+][0-9]" etc))
                (primary-p (ppcre:scan-to-strings "primary" etc))
                (modes (loop
                         as line = (car lines)
                         while (ppcre:scan "^ +[0-9]+x[0-9]+" line)
                         collect (xrandr-parse-mode (funcall pop-line t))))
                (features (ppcre:split " +" features-string))
                (extra (loop
                         while (ppcre:scan "^ +" (car lines))
                         collect (funcall pop-line))))
            (make-xrandr-display
             :raw-line line
             :id id :state state
             :modes modes
             :connected-p
             (cond
               ((equal state "connected") t)
               ((equal state "disconnected") nil)
               (t (error "invalid state value: ~A" state)))
             :current-mode (loop for mode in modes
                                   thereis (when (xrandr-mode-active mode)
                                             mode))
             :primary-p primary-p
             :features features
             :inverted-p (member "inverted" features :test #'equal)
             :mode (loop for mode in modes thereis
                                           (and (xrandr-mode-active mode) mode)))))
    do (assert display)
    collect display))

(defun xdisplays-persist-preference
    (xrandr-pref
     &key
       (prefs-file #P"~/.xdisplays"))
  (with-slots (display-mode order) xrandr-pref
    (assert (xor display-mode order))
    (let (line-id value proc output status)
      (cond
        (display-mode
         (destructuring-bind (display . mode) display-mode
           (setf line-id (format nil "#mode-for-~A" display)
                 value (format nil "mode ~A ~A" display mode))))
        (order
         (setf
          value (format nil "order ~{~A~^ ~}"
                        order)
          line-id (format nil "#order-for-~{~A~^-~}"
                          (sort (copy-list order) #'string<))))
        (t (error "nothing to persist")))
      (setf output
            (with-output-to-string (out-fh)
              (with-input-from-string (in-fh value)
                (setf proc (sb-ext:run-program
                            "insert-text-block"
                            (list line-id
                                  (uiop:native-namestring prefs-file))
                            :wait t
                            :search t
                            :input in-fh
                            :output out-fh)))))
      (setf status (sb-ext:process-exit-code proc))
      (unless (zerop status)
        (error "non-zero exit status: ~A" output)))))

(defun xdisplays-parse-preferences (&key (prefs-file #P"~/.xdisplays"))
  (when (probe-file prefs-file)
    (with-open-file (fh prefs-file
                        :direction :input)
      (loop as line = (read-line fh nil nil)
            while line
            unless (or (equal "" line)
                       (STARTS-WITH-SUBSEQ "#" line))
              collect
              (destructuring-bind (pref-type . value)
                  (cl-ppcre:split #\Space line)
                (cond
                  ((equal pref-type "mode")
                   (destructuring-bind (display mode) value
                     (make-xrandr-pref :display-mode
                                       (cons display mode))))
                  ((equal pref-type "order")
                   (make-xrandr-pref :order value))
                  (t (error "unparseable pref line: ~A" line))))))))

(defun xrandr-connected-displays ()
  (remove-if-not 'xrandr-display-connected-p (xrandr-displays)))

(defun correct-screen (&key order skip-off-commands)
  "Order is a list of indices into the current (xrandr-connected-displays)."
  (let* ((displays (xrandr-displays))
         (connected (xrandr-connected-displays))
         (to-connect-ordered (or (loop for ith in order
                                       collect
                                       (if (numberp ith)
                                           (nth ith connected)
                                           ith))
                                 connected))
         (to-connect-ids (mapcar #'xrandr-display-id to-connect-ordered))
         (to-disconnect (remove-if (lambda (display)
                                     (member (xrandr-display-id display)
                                             to-connect-ids
                                             :test #'equal))
                                   displays))
         cmd)
    ;; disconnect
    (unless skip-off-commands
      (loop for off-display in to-disconnect
            do (setf cmd
                     (nconc cmd (list
                                 "--output"
                                 (xrandr-display-id off-display)
                                 "--off")))))
    ;; connect displays in order
    (loop for display in to-connect-ordered
          with pos-x = 0
          with prefs = (xdisplays-parse-preferences)
          ;; with cmd = nil
          as display-id = (xrandr-display-id display)
          as mode =
                  (or
                   (loop for pref in prefs
                         as display-mode = (xrandr-pref-display-mode pref)
                           thereis
                           (and display-mode
                                (destructuring-bind (display . mode)
                                    display-mode
                                  (when (equal display-id display)
                                    (xrandr-parse-mode mode)))))
                   (car (xrandr-display-modes display)))
          as args =
                  (let* ((id (xrandr-display-id display))
                         (mode-string (xrandr-mode-resolution-string mode))
                         (pos-string (format nil "~Dx0" pos-x)))
                    (list
                     "--output" id
                     "--mode" mode-string
                     "--pos" pos-string))
          do (setf cmd (nconc cmd args))
          do (incf pos-x (xrandr-mode-width mode)))
    (run-command-sync-notify-on-error "xrandr" cmd)))

(defcommand correct-screen-prompt-display-order () ()
  "Correct screen, prompting for display order"
  (let* ((displays (xrandr-connected-displays))
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
                      collect (- (char-code c) (char-code #\0)))))
    (when (and order
               (>= (apply 'max order) (length displays)))
      (error "index out of bounds"))
    (correct-screen :order order)
    (when (cdr order)
      (xdisplays-persist-preference
       (make-xrandr-pref
        :order (loop for idx in order
                     collect (xrandr-display-id (nth idx displays))))))))

(defcommand correct-screen-swap-display-order () ()
  "Swap the display order. Requires exactly 2 displays connected."
  (let* ((displays (xrandr-connected-displays)))
    (correct-screen :order '(1 0))))

(defun xrandr-display-order-key (ids)
  (format nil "~{~A~^ ~}" (sort (copy-list ids) #'string<)))

(defcommand correct-screen-all-connected-displays
    () ()
  "correct screen without prompt"
  (let* ((connected (xrandr-connected-displays))
         (ids (mapcar #'xrandr-display-id connected))
         (key
           (xrandr-display-order-key ids))
         (ordered-ids
           (or
            (loop for pref in (xdisplays-parse-preferences)
                  as order = (xrandr-pref-order pref)
                    thereis
                    (when (and
                           order
                           (equal key
                                  (xrandr-display-order-key order)))
                      order))
            ids))
         (index (loop for id in ids
                      for i from 0
                      collect (cons id i)))
         (order (loop for id in ordered-ids
                      collect (cdr (assoc id index :test #'equal)))))
    (message-wrapped "~D output~:P detected" (length order))
    (correct-screen :order order)))

(defcommand correct-screen-newly-connected-displays
    () ()
  "only add newly connected displays"
  (let ((new-displays (remove-if-not (lambda (display)
                                       (and (xrandr-display-connected-p display)
                                            (null (xrandr-display-mode display))))
                                     (xrandr-connected-displays))))
    (message-wrapped "~D new output~:P detected" (length new-displays))
    (when new-displays
      (correct-screen :order (xrandr-connected-displays)
                      :skip-off-commands t))))


(defcommand correct-screen-only-current-display
    () ()
  "turn off all other displays"
  (let* ((head-number (head-number (current-head)))
         (head (nth head-number (xrandr-displays))))
    (message-wrapped "selecting only display: ~A" head-number)
    (correct-screen :order (list head))))

(defcommand correct-screen-select-mode () ()
  "select a mode for current displays"
  (let* ((display (selcand:select :candidates
                                  (xrandr-connected-displays)
                                  :prompt "select display: "
                                  :stringify-fn #'XRANDR-DISPLAY-ID))
         (all-modes (XRANDR-DISPLAY-MODES display))
         (current-mode (xrandr-display-current-mode display))
         (similar-modes (loop for mode in all-modes
                              with ratio = (xrandr-mode-ratio current-mode)
                              when (equal ratio (xrandr-mode-ratio mode))
                                collect mode))
         (mode (selcand:select :candidates (or similar-modes all-modes)
                               :prompt "select mode: "
                               :stringify-fn
                               (lambda (mode)
                                 (with-slots (width height active) mode
                                   (format nil "~A~Dx~D (~,2F)"
                                           (if active "*" "")
                                           width height
                                           (/ width height))))
                               :display-candidates t
                               :columns 3))
         (mode-string (with-slots (width height active) mode
                        (format nil "~Dx~D"
                                width height))))
    (run-command-async
     "xrandr"
     (list "--output"
           (XRANDR-DISPLAY-ID display)
           "--mode" mode-string))
    (xdisplays-persist-preference
     (make-xrandr-pref
      :display-mode
      (cons (xrandr-display-id display) mode-string)))))

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
  (run-shell-command "invert-colors-toggle.sh"))

(defcommand correct-screen-refresh-displays () ()
  "wake up an HDMI display"
  (correct-screen-only-current-display)
  (correct-screen-all-connected-displays))
