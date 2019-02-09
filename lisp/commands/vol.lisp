(defvar *vol-muted* nil)
(defvar *vol-backend* :amixer)

(setf *vol-backend*
      (cond
        ((which "amixer") :amixer)
        ((which "pactl") :pactl)
        (t (warn "no volume cli found"))))

(defun vol (action
            &key
              (percent 3)
              sink
              (backend *vol-backend*))
  (assert (member action '(:set :up :down :mute-toggle :get)))
  (case backend
    (:amixer
     (setf sink (or sink "Master"))
     (case action
       ((:set :up :down)
        (sb-ext:run-program
         "amixer"
         (list "set"
               sink
               (format nil "~D%~A" percent
                       (case action
                         (:set "")
                         (:up "+")
                         (:down "-"))))
         :wait nil
         :search t))
       (:mute-toggle
        (if *vol-muted*
            (progn (vol :set :percent *vol-muted*)
                   (setf *vol-muted* nil)
                   t)
            (progn (setf *vol-muted* (vol :set))
                   (vol :set :percent 0)
                   nil)))
       (:get
        (let* ((out (run-shell-command (format nil "amixer get ~A" sink) t))
               (vol (ppcre:register-groups-bind (vol) ("Playback.*?[[]([0-9]+)%]" out)
                      vol)))
          (assert vol)
          vol))))
    (:pactl
     (setf sink (or sink 0))
     (case action
       ((:set :up :down)
        (sb-ext:run-program
         "pactl"
         (list "set-sink-volume"
               (write-to-string sink)
               (format nil "~A~D%"
                       (case action
                         (:set "")
                         (:up "+")
                         (:down "-"))
                       percent))
         :wait nil
         :search t))
       (:get (error "not implemented"))
       (:mute-toggle (error "not implemented"))))
    (t (error "unknown backend ~A" backend))))

(defcommand vol-up ()() "volume up"
            (vol :up)
            (message "volup"))

(defcommand vol-down ()() "volume down"
            (vol :down)
            (message "voldown"))

(defcommand vol-mute-toggle ()() "volume mute"
            (let ((muted (vol :mute-toggle)))
              (message "vol ~Amute"
                       (if muted "" "un"))))
