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

(defcommand speak-string (text)
    ((:string "enter string to speak: " ))
  "speak some text"
  (SB-EXT:RUN-PROGRAM "espeak"
		      (list text)
		      :search t
		      :output t
		      :error t
		      :wait t))

(defcommand spell-clipboard () ()
  "spell clipboard contents, character by character"
  (speak-string
   (cl-ppcre:regex-replace-all "(.)" (GET-X-SELECTION) " \\1")))
