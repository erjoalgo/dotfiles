(in-package :stumpwm)

(defvar *vol-muted* nil)

(defun vol-find-backend ()
  (cond
    ((which "pactl") :pactl)
    ((which "amixer") :amixer)
    (t (warn "no volume cli found"))))

(defparameter *vol-backend* (vol-find-backend))
(defparameter *audio-default-sink* nil)

(defun pulseaudio-sink-indices ()
  (or
   (let* ((output (run-shell-command "pactl list sinks | grep -Po '(?<=^Sink #)[0-9]+'" t))
          (sinks (ppcre:split #\Newline output)))
     sinks)
   (let ((output (run-shell-command "pacmd list-sinks" t))
         indices)
     (ppcre:do-register-groups ((#'parse-integer index)) ("(?m)^ +[*] +index: ([0-9]+)" output)
       (push index indices))
     (reverse indices))))

(defstruct audio-sink
  index
  description
  muted
  ports)

(defun audio-parse-sinks ()
  (let ((output (run-shell-command "pactl list sinks" t))
        sinks)
    (ppcre:do-register-groups (index description muted ports)
        ("(?sm)^Sink #([0-9]+).*?Description: ([^
]+).*?Mute: +([^
]+).*?Ports:[
 	]+([^
]+)" output)
      (push (make-audio-sink :index index :description description
                             :muted (cond ((equal muted "yes") t)
                                          ((equal muted "no") nil)
                                          (t (error "unknown mute value: ~A" muted)))
                             :ports ports) sinks))
    sinks))


(defun audio-select-sink ()
  (selcand:select :candidates
                  (audio-parse-sinks)
                  :prompt "select audio sink: "
                  :stringify-fn #'AUDIO-SINK-PORTS
                  :display-candidates t
                  :autoselect-if-single nil))

(defun audio-get-default-sink (&key force)
  (setf *audio-default-sink*
        (or (unless force *audio-default-sink*)
            (car (audio-parse-sinks))
            (error "no pulseaudio sinks found"))))

(defcommand audio-set-default-sink () ()
  "set the default sink"
  (let ((sink (audio-select-sink)))
    (setf *audio-default-sink* sink)
    (run-command-async "pactl" (list "set-default-sink" (audio-sink-index sink))
                       t)))

(defun vol (action
            &key
              (percent 3)
              (alsamixer-control "Master")
              (backend *vol-backend*))
  (assert (member action '(:set :up :down :mute-toggle :get)))
  (assert backend)
  (case backend
    (:amixer
     (case action
       ((:set :up :down)
        (sb-ext:run-program
         "amixer"
         (list "set"
               alsamixer-control
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
        (let* ((out (run-shell-command (format nil "amixer get ~A" alsamixer-control) t))
               (vol (ppcre:register-groups-bind (vol) ("Playback.*?[[]([0-9]+)%]" out)
                      vol)))
          (assert vol)
          vol))))
    (:pactl
     (case action
       ((:set :up :down)
        (sb-ext:run-program
         "pactl"
         (list "set-sink-volume"
               (format nil "~D" (audio-sink-index (audio-get-default-sink)))
               (format nil "~A~D%"
                       (case action
                         (:set "")
                         (:up "+")
                         (:down "-"))
                       percent))
         :wait nil
         :search t))
       (:get (error "not implemented"))
       (:mute-toggle
        (let ((sink (audio-get-default-sink)))
          (sb-ext:run-program
           "pactl"
           (list "set-sink-mute"
                 (format nil "~D" (audio-sink-index sink))
                 "toggle")
           :wait nil
           :search t)
          (audio-sink-muted (audio-get-default-sink :force t))))))
    (t (error "unknown backend ~A" backend))))

(defcommand vol-up ()() "volume up"
  (vol :up)
  (message-wrapped "volup"))

(defcommand vol-down ()() "volume down"
  (vol :down)
  (message-wrapped "voldown"))

(defcommand vol-mute-toggle ()() "volume mute"
  (let ((muted (vol :mute-toggle)))
    (message-wrapped "vol ~Amute"
                     (if muted "" "un"))))
