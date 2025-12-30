(in-package :stumpwm)

(defcommand speak-key ()
    ;;((:key "enter key to speak: " ));;this requires pressing enter
    ()
  ;;(let ((text (key-keysym key)))
  "speak the name of the next typed key"
  (let ((text (read-one-char (current-screen))))
    (SB-EXT:RUN-PROGRAM "espeak"
			(list (format nil "~A" text))
			:search t
			:output t
			:error t
			:wait t)))

(defun speak-string (text &key (speed-wpm 160) (word-gap-ms 10))
  (run-command-async
   "espeak"
   (list (format nil "-s~D" speed-wpm)
         (format nil "-g~D" word-gap-ms)
         text)
   t))

(defcommand speak-string-cmd (text)
    ((:string "enter string to speak: " ))
  "speak some text"
  (speak-string text))

(defcommand spell-clipboard () ()
  "spell clipboard contents, character by character"
  (let* ((word (GET-X-SELECTION)))
    (spell-word word)))

(defparameter spell-words
  '("Yokohama" "Guadalajara" "Atlanta"
    "Houston"
    "Jerusalem" "Paris" "India"
    "Beijing" "Chicago" "Xochimilco" "Florence"
    "Richmond" "Detroit" "Queretaro"
    "Washington" "Oakland" "Korea"
    "Zero"
    "Egypt" "Uruguay" "Vietnam"
    "London" "Shanghai" "Mexico"
    "New York" "Tokyo"))

(defparameter char-sample-word
  (loop
    with alist = nil
    for word in spell-words
    as c = (aref word 0)
    as key = (CHAR-DOWNCASE c)
    as val = (assoc key alist)
    unless (or val (not (ALPHA-CHAR-P c)))
      do (push (cons key word) alist)
    finally (return alist)))

(defun spell-char-line (c)
  (cond
    ((ALPHA-CHAR-P c)
     (let* ((c-lower (char-downcase c))
            (word (cdr (assoc c-lower char-sample-word)))
            (is-capital (not (eq c-lower c))))
       (format nil "~A ~A as in ~A"
               (if is-capital "capital" "")
               (cond ((eq c-lower #\a) "'A'")
                     (t (format nil "'~C'" (char-upcase c))))
               (or word
                   (progn
                     (warn "missing sample word for char ~C" c-lower)
                     (format nil "~C" c-lower))))))
    ((DIGIT-CHAR-P c)
     (format nil "~C" c))
    ((eq c #\Space) "Space")
    (t (format nil "the symbol ~C" c))))

(defun spell-word (word)
  (let* ((spelling-lines
           (loop for c across word collect (spell-char-line c))))
    (sleep 1)
    (speak-string (format nil "~{~A~^, ~}" spelling-lines) :word-gap-ms 30)
    spelling-lines))
