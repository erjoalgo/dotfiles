;; override render-strings to fix concurrency issues

(defvar *message-lock* (sb-thread:make-mutex :name "echo-mutex"))

(defun render-strings (cc padx pady strings highlights)
  (sb-thread:with-mutex (*message-lock*)

    (let* ((gc (ccontext-gc cc))
           (xwin (ccontext-win cc))
           (px (ccontext-px cc))
           (strings (mapcar (lambda (string)
                              (if (stringp string)
                                  (parse-color-string string)
                                  string))
                            strings))
           (y 0))
      ;; Create a new pixmap if there isn't one or if it doesn't match the
      ;; window
      (when (or (not px)
                (/= (xlib:drawable-width px) (xlib:drawable-width xwin))
                (/= (xlib:drawable-height px) (xlib:drawable-height xwin)))
        (if px (xlib:free-pixmap px))
        (setf px (xlib:create-pixmap :drawable xwin
                                     :width (xlib:drawable-width xwin)
                                     :height (xlib:drawable-height xwin)
                                     :depth (xlib:drawable-depth xwin))
              (ccontext-px cc) px))
      ;; Clear the background
      (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
        (xlib:draw-rectangle px gc 0 0
                             (xlib:drawable-width px)
                             (xlib:drawable-height px) t))
      (loop for parts in strings
         for row from 0 to (length strings)
         for line-height = (max-font-height parts cc)
         if (find row highlights :test 'eql)
         do (xlib:draw-rectangle px gc 0 (+ pady y) (xlib:drawable-width px) line-height t)
           (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc)
                                   :background (xlib:gcontext-foreground gc))
             ;; If we don't switch the default colors, a color operation
             ;; resetting either color to its default value would undo the
             ;; switch.
             (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc))
             (render-string parts cc (+ padx 0) (+ pady y))
             (rotatef (ccontext-default-fg cc) (ccontext-default-bg cc)))
         else
         do (render-string parts cc (+ padx 0) (+ pady y))
         end
         do (incf y line-height))
      (xlib:copy-area px gc 0 0
                      (xlib:drawable-width px)
                      (xlib:drawable-height px) xwin 0 0)
      (reset-color-context cc)
      (values))))
