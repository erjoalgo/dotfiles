(in-package :STUMPWM)

;;to avoid sync-keys on every define-key
(defvar *real-top-map* nil)

(setf *real-top-map* *top-map*)

(define-run-or-pull-program "BROWSER"
  :cmd (format nil "~{~A~^ ~}" *browser-cmd*)
  :raise-key "H-f"
  :pull-key "H-F"
  :classes *browser-classes*)

(define-run-or-pull-program "X-TERMINAL-EMULATOR"
  :raise-key "H-c"
  :cmd (trim-spaces
        (run-shell-command
         "which konsole roxterm gnome-terminal xterm | head -1" t))
  :classes (list "Konsole" "X-terminal-emulator" "Roxterm" "roxterm"
		 "xterm" "XTerm" "Gnome-terminal"))

(define-run-or-pull-program "emacs"
  :pull-key "H-E"
  :classes emacs-classes)

(let ((eclipse-cmd
        (first-existing-command
         "eclipse"
         "android-studio"
         "STS"
         "studio.sh")))
  (when eclipse-cmd
    (define-run-or-pull-program "android-studio"
      :classes '("jetbrains-studio" "Spring Tool Suite" "Eclipse")
      :cmd (list "sudo" "-u" "ealfonso" eclipse-cmd)
      :raise-key "H-t H-r"
      :pull-key "H-t H-R")))

(define-run-or-pull-program "zathura"
  :classes '("Zathura")
  :cmd "zathura"
  :raise-key "H-t H-z"
  :pull-key "H-t H-z")

(define-run-or-pull-program "virtualbox"
  :classes '("VirtualBox Machine" "VirtualBox Manager" "Virt-manager")
  :cmd "virt-manager"
  :raise-key "H-t H-v"
  :pull-key "H-t H-V")

(define-run-or-pull-program "pavucontrol"
  :classes '("Pavucontrol" )
  :cmd "pavucontrol"
  :pull-key "H-O"
  :raise-key "H-o")

(define-run-or-pull-program "emulator-run.sh"
  :raise-key "H-t H-e"
  :pull-key "H-t H-E"
  :cmd "emulator-run.sh"
  :classes '("eboard" "Eboard"))

(define-run-or-pull-program "signal-desktop"
  :raise-key "H-t H-s"
  :pull-key "H-t H-S"
  :classes '("signal" "Signal"))

(define-run-or-pull-program "blender"
  :raise-key "H-t H-b"
  :pull-key "H-t H-B"
  :classes '("Blender" "OpenSCAD"))

(define-run-or-pull-program "ffplay"
  :cmd nil
  :raise-key "H-t H-f"
  :pull-key "H-t H-F"
  :classes '("ffplay" "ffplay"))

(define-run-or-pull-program "gimp"
  :cmd nil
  :raise-key "H-t H-g"
  :pull-key "H-t H-g"
  :classes '("Gimp"))

(defun last-modified-file (dir ext)
  (let* ((cmd (format nil
                      "find-last-modified-file-fast.sh -d \"~A\" -e ~A"
                      (truename dir)
                      ext))
         (output (run-shell-command cmd t))
         (lines (cl-ppcre:split #\Newline output)))
    (car lines)))

(define-run-or-pull-program "creality"
  :raise-key "H-t H-c"
  :pull-key "H-t H-C"
  :classes '("cura" "Creative3D" "Creality Print" "UltiMaker-Cura")
  :cmd (lambda ()
         (list
          (namestring (or
                       (car (directory #P"~/bin/Creality*.AppImage"))
                       "cura"))
          (last-modified-file "~/git/3d" "stl"))))

(define-run-or-pull-program "xournal"
  :raise-key "H-t H-x"
  :pull-key "H-t H-x"
  :classes '("Xournal")
  :cmd nil)

(define-run-or-pull-program "vnc"
  :raise-key "H-t H-t"
  :pull-key "H-t H-t"
  :classes '("Vncviewer")
  :cmd nil)

(define-run-or-pull-program "n64"
  :raise-key "H-t H-n"
  :pull-key "H-t H-n"
  :classes '("project64.exe" "mupen64plus")
  :cmd
  (cond
    ((which "mupen64plus")
     (list "mupen64plus" (n64-select-rom)))
    ((which "wine")
     (list "wine"
           (uiop:native-namestring
            #P"~/.wine/drive_c/Program Files (x86)/Project64 3.0/Project64.exe")))))

(define-run-or-pull-program "ledger-live"
  :raise-key "H-t H-l"
  :pull-key "H-t H-l"
  :classes '("Ledger Live")
  :cmd
  (cond
    ((which "ledger-live")
     "ledger-live")
    (t
     (error "ledger live not found"))))

(define-run-or-pull-program "linphone"
  :raise-key "H-t H-p"
  :pull-key "H-t H-p"
  :classes '("linphone")
  :cmd "linphone")

(define-run-or-pull-program "audacity"
  :raise-key "H-t H-y"
  :pull-key "H-t H-y"
  :classes '("Audacity")
  :cmd "audacity")

(define-run-or-pull-program "scrcpy"
  :raise-key "H-t H-a"
  :pull-key "H-t H-A"
  :run-key "H-t C-H-a"
  :classes '("scrcpy")
  :cmd (lambda ()
         (let ((device-id (adb-select-device)))
           (list "scrcpy" "-s" device-id "--shortcut-mod=lalt" "-S"))))

(define-run-or-pull-program "wireshark"
  :raise-key "H-t H-w"
  :pull-key "H-t H-w"
  :classes '("Wireshark")
  :cmd "wireshark")

(define-run-or-pull-program "tor-browser"
  :cmd '(#P"~/src/tor-browser/Browser/start-tor-browser")
  :raise-key "H-t H-t"
  :pull-key "H-t H-T"
  :classes '("Tor Browser"))

(define-run-or-pull-program "gqrx"
  :cmd '("gqrx")
  :raise-key "H-t H-q"
  :pull-key "H-t H-Q"
  :classes '("gqrx"))

(define-run-or-pull-program "magnus"
  :cmd '("magnus")
  :raise-key "H-t H-m"
  :pull-key "H-t H-M"
  :classes '("Magnus"))

(define-run-or-pull-program "vmpk"
  :cmd '("vmpk")
  :raise-key "H-t H-k"
  :pull-key "H-t H-K"
  :classes '("VMPK"))

(per-window-bindings-reload-from-fn)

(push-top-map (make-sparse-keymap))

(defun define-key-bindings (kmap-or-kmap-list bindings)
  (loop with kmap-list = (if (listp kmap-or-kmap-list)
			     kmap-or-kmap-list
			     (list kmap-or-kmap-list))
        for kmap in kmap-list do
          (loop for (key action) in bindings
                as kbd = (if (stringp key) (kbd key)
                             key)
                do
	           (define-key kmap kbd action))
        finally (return kmap)))


(defmacro def-several-vars (value-form &rest vars)
  `(progn ,@(loop for var in vars collect `(defvar ,var ,value-form))))

(def-several-vars
    (make-sparse-keymap)
  *screen-rotation-map*
  *utils-map*
  *special-chars-map*
  ;; *search-engine-map*
  *commands-map*
  *brightness-map*
  *correct-screen-map*
  *click-map*)

(defun all-top-maps ()
  (append (list *real-top-map*)
	  (when *per-window-bindings-class-to-map*
	    (loop for map being the hash-value of *per-window-bindings-class-to-map*
	          collect map))))

(define-key-bindings
    (all-top-maps)
    `(
      ("XF86PowerOff" "echo power pressed root!")
      ;;brightness
      ;;emergency dimming
      ("XF86MonBrightnessDown" "brightness-down")
      ("XF86MonBrightnessUp" "brightness-up")
      ;;("XF86AudioLowerVolume" nil)
      ;;("XF86AudioRaiseVolume" nil)

      ("H-P" "other-frame-scroll-browser w")
      ("H-N" "other-frame-scroll-browser d")

      ;;("H-m" "raise-remmina")
      ;;("H-m" "raise-matlab")
      ;;("H-p" "raise-pidgin")
      ("Scroll_Lock" "fnext")
      ("H-a" "echo-date-battery")
      ("H-w" "echo-windows-with-group")

      ("H-k" "delete")
      ("H-K" "kill")
      ("H-z" "stop-window")
      ("H-Z" "cont-window")
      ("M-H-f" "chrome-restart")
      ("H-;" "colon")
      ("H-:" "eval")
      ("H-x" "exec")
      ("H-X" *commands-map*)
      ("C-;" "run-shell-command pgrep keynav || keynav-restart")
      ;; ("H-z" *commands-map*)
      ("H-u" *utils-map*)
      ("F12" "run-shell-command find-cursor")
      ("S-F12" "private-command")

      ("H-/" "pull-hidden-other")
      ("H--" "fclear")
      ("H-TAB" "fnext")

      ("H-h" *help-map*)

      ("H-DEL" *special-chars-map*)
      ("H-I" "invert-current-window")
      ("H-i" "invert-screen")

      ("H-p" "pull-hidden-previous")
      ("H-n" "pull-hidden-next")

      ;; ("H-g" "search-engine-search baidu")
      ("H-g" "search-engine-search ddg")

      ("H-m" *search-engine-map*)
      ("H-l" "launch-url")
      ("H-L" "launcher-append-url")
      ("H-Home" "asdfasdf")
      ("H-ISO_Left_Tab" "move-window-toggle")


      ("H-1" "select-window-by-number 1")
      ("H-2" "select-window-by-number 2")
      ("H-3" "select-window-by-number 3")
      ("H-4" "select-window-by-number 4")
      ("H-5" "select-window-by-number 5")
      ("H-6" "select-window-by-number 6")
      ("H-7" "select-window-by-number 7")
      ("H-8" "select-window-by-number 8")
      ("H-9" "select-window-by-number 9")
      ("H-0" "select-window-by-number 0")

      ("H-!" "pull-window-by-number 1")
      ("H-@" "pull-window-by-number 2")
      ("H-#" "pull-window-by-number 3")
      ("H-$" "pull-window-by-number 4")
      ("H-%" "pull-window-by-number 5")
      ("H-^" "pull-window-by-number 6")
      ("H-&" "pull-window-by-number 7")
      ("H-*" "pull-window-by-number 8")
      ("H-(" "pull-window-by-number 9")
      ("H-)" "pull-window-by-number 0")


      ("M-H-1" "only")
      ("M-H-2" "vsplit")
      ("M-H-3" "hsplit")
      ("M-H-q" "remove")

      ;;volume
      ("XF86AudioMute" "vol-mute-toggle")
      ("S-XF86AudioLowerVolume" "vol-mute-toggle")
      ("XF86AudioLowerVolume" "vol-down")
      ("XF86AudioRaiseVolume" "vol-up")
      ("S-XF86AudioRaiseVolume" "audio-set-default-sink")

      ("H-S-F9" "vol-mute-toggle")
      ("H-F9" "vol-down")
      ("H-F10" "vol-up")
      ("H-S-F10" "audio-set-default-sink")

      ("H-N" "gnext")
      ("H-P" "gprev")
      ("H-B" "sip-call-dtmf")
      ("H-b" "sip-main")
      ("H-V" "paste-primary")
      ("H-T" "openproject-create-personal-task")
      ("H-q" "run-last-kbd-macro")
      ("H-Q" "run-kbd-macro")
      (,(make-key :keysym 65258) "middle-click")
      (,(make-key :keysym 65535) "middle-click")
      ("H-r" "redshift-shift-red")
      ("H-R" "redshift-shift-blue")
      ("H-`" "unmap-all-message-windows-command")
      ("H-s" *press-ir-button-map*))
  ;;not efficient nor necessary but only run at initialization
  )



(defcommand gselect-nth (i) ((:number "enter group number: "))
  (assert (< i (length (sort-groups (current-screen)))))
  '(switch-to-group group-name)
  (-> i
      (nth (sort-groups (current-screen)))
      switch-to-group))

(defcommand gmove-nth (i) ((:number "enter group number: "))
  (assert (< i (length (sort-groups (current-screen)))))
  (let ((group (nth i (sort-groups (current-screen)))))
    (gmove group)))

(loop
  for group-name in '("Default" "F2" "F3" "F4" "F5")
  for i from 0
  do (gnewbg group-name)
  do
     (define-key-bindings (all-top-maps)
         `(
           (,(format nil "H-F~D" (1+ i)) ,(format nil "gselect-nth ~D" i))
           (,(format nil "S-H-F~D" (1+ i)) ,(format nil "gmove-nth ~D" i)))))

(define-key-bindings
    *correct-screen-map*
    `(
      ("H-p" "correct-screen-prompt-display-order")
      ("H-a" "correct-screen-all-connected-displays")
      ("H-n" "correct-screen-newly-connected-displays")
      ("H-1" "correct-screen-only-current-display")
      ("H-m" "correct-screen-select-mode")
      ("H-r" "correct-screen-refresh-displays")))

(define-key-bindings
    *utils-map*
    '(
      ("i" "invert-screen")
      ("M" "echo-pointer")
      ("m" "magnus")
      ("H-c" *correct-screen-map*)
      ("r" *screen-rotation-map*)
      ("c" *special-chars-map*)
      ("H-w" "connect-to-internet-maybe")
      ("w" "connect-to-internet-maybe")
      ("h" *help-map*)
      ("s" "scrot-cmd-anon")
      ("S" "scrot-cmd-current-window-anon")
      ("n" "take-scrot-snipit")
      ("o" "ocr-scrot-clipboard")
      ("O" "spell-clipboard")
      ("k" "speak-string")
      ("l" "spell-clipboard")
      ("H-l" "screen-lock-interactive")
      ("l" "screen-lock-interactive")
      ;; ("H-e" "echo-current-tab" )
      ("H-e" "emacs-killusr2")
      ("H-E" "emacs-killusr2-tmux")
      ("z" "window-sleep-toggle")
      ("H-t" "tmp")
      ("H-b" "byzanz-record-auto")
      ("H-B" "byzanz-record-auto-stop")
      ("H-s" "toggle-screen-key")
      ("b" *brightness-map*)
      ("H-p" "ledger-password-backup-restore")
      ("H-P" "ledger-password-type")
      ("p" "center-pointer")
      ("d" "dict-lookup-command")
      ("D" "cat-message-command ~/vocab")
      ("H-v" "pulseaudio-load-module-loopback")
      ("H-V" "pulseaudio-unload-module-loopback")
      ("H-m" *click-map*)))


(define-key-bindings *screen-rotation-map*
    '(
      ("a" "rotate-screen left")
      ("d" "rotate-screen right")
      ("w" "rotate-screen normal")
      ("s" "rotate-screen inverted")))

(define-key-bindings
    *special-chars-map*
    `(
      ("DEL" "insert-key-with-delay grave")
      ;;("3" "insert-key-with-delay grave 3")
      ("?" "insert-key-with-delay questiondown")
      ("u" "insert-key-with-delay udiaeresis")
      ("n" "insert-key-with-delay ntilde")
      ("!" "run-shell-command xdotool key exclamdown")
      ("~" "insert-key-with-delay degree")
      ("$" "insert-key-with-delay EuroSign")
      ))

(define-key-bindings
    *click-map*
    `(("H-d" "double-click-and-ctrl-c")))


(define-key *help-map* (kbd "g") "echo-current-group-name")
;;(define-key *special-characters-map* (kbd "?") "run-shell-command xdotool key ¿")

(define-key-bindings
    *brightness-map*
    (append
     (loop for c across "`1234567890"
           for percentage in '(.01 .05 .1 .5 .8
                               1 30 50 70 90
                               100)
           collect
           (list (format nil "~C" c)
                 (format nil "set-brightness ~A" percentage)))
     `(("r" "redshift-shift-red")
       ("b" "redshift-shift-blue")
       ("x" "redshift-reset"))))

(defun key-equal (a b)
  (equal (print-key a) (print-key b)))

(defun run-keymap-repeatedly (keymap-name keymap &key (quit-key (kbd "ESC")))
  (loop
    as (cmd . seq) = (multiple-value-bind (cmd seq)
                         (with-focus (screen-key-window (current-screen))
                           (message "reading key from ~A~%" keymap-name)
                           (read-from-keymap (list keymap)))
                       (cons cmd seq))
    while
    (cond
      ((and (null (cdr seq))
            (key-equal (car seq) quit-key))
       (message "quit key pressed. quitting...")
       nil)
      ((null cmd) (message "no comannd bound. quitting...")
       nil)
      (t
       (message "calling ~A~%" cmd)
       (eval-command cmd)
       (sleep .5)
       t))))

(defmacro run-keymap-repeatedly-command (keymap)
  (let ((cmd-name (format nil "CALL-REPEATEDLY-~A" keymap)))
    `(progn
       (defcommand ,(intern cmd-name) () ()
         (run-keymap-repeatedly ,(format nil "~A" keymap) ,keymap))
       ,cmd-name)))


(defmacro define-tv-buttons (brand-name brand-keymap &optional extra)
  (loop
    for (l action) in
    `(("p" "POWER")
      ("P" "POWER_OFF")

      ("Up" "UP")
      ("Down" "DOWN")
      ("Left" "LEFT")
      ("Right" "RIGHT")

      ("Return" "ENTER")
      ("SPC" "ENTER")

      ("i" "INPUT")
      ("c" "PICTURE")
      ("m" "MENU")
      ("h" "HOME")
      ("q" "EXIT")
      ("x" "EXIT")
      ("DEL" "BACK")

      ("XF86AudioLowerVolume" "VOLDOWN")
      ("XF86AudioRaiseVolume" "VOLUP")
      ("XF86AudioMute" "MUTE")

      ("S-Up" "PICTURE_MODE_UP")
      ("S-Down" "PICTURE_MODE_DOWN"))
    as command = (format nil "press-ir-button ~A_~A" brand-name action)
    collect `(list ,l ,command) into buttons
    finally
       (return
         `(progn
            (defparameter ,brand-keymap (make-sparse-keymap))
            (define-key-bindings
                ,brand-keymap
                (list ,@buttons ,@extra))
            ,brand-keymap))))



(defparameter *press-ir-button-map* (make-sparse-keymap))

(define-key-bindings
    *press-ir-button-map*
    `(
      ("v" ,(define-tv-buttons "VIZIO" *ir-buttons-vizio-tv*))
      ("V" ,(run-keymap-repeatedly-command *ir-buttons-vizio-tv*))

      ("d" ,(define-tv-buttons "DYNEX" *ir-buttons-dynex-tv*))

      ("c" ,(define-tv-buttons "TCL" *ir-buttons-tcl-tv*))
      ("C" ,(run-keymap-repeatedly-command *ir-buttons-tcl-tv*))

      ("s" ,(define-tv-buttons "SOUNDBAR" *ir-buttons-vizio-soundbar*))
      ("S" ,(run-keymap-repeatedly-command *ir-buttons-vizio-soundbar*))

      ("a" ,(define-tv-buttons "AC" *ir-buttons-ac*))
      ("A" ,(run-keymap-repeatedly-command *ir-buttons-ac*))

      ("l" ,(define-tv-buttons "LG" *ir-buttons-lg-tv*))
      ("L" ,(run-keymap-repeatedly-command *ir-buttons-lg-tv*))

      ("g" "garage-door-toggle")

      ("t" ,(define-key-bindings
                (make-sparse-keymap)
                (append
                 (loop with tvs = '(
                                    ;; "VIZIO"
                                    "TCL"
                                    "LG")
                       for (key button-fmt) in
                       `(("Up" "~A_PICTURE_MODE_UP")
                         ("Down" "~A_PICTURE_MODE_DOWN")
                         ("p" "~A_POWER"))
                       collect (list key
                                     (format nil "press-ir-button ~{~A~^,~}"
                                             (mapcar (lambda (tv) (format nil button-fmt tv)) tvs))))
                 `(
                   ("w" "press-ir-button LG_POWER,TCL_ENTER"))
                 )))))



(set-prefix-key (kbd "F19"))
(pop-top-map)
;; TODO use buttons framework
