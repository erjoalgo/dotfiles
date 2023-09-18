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
        "STS")))
  (when eclipse-cmd
    (define-run-or-pull-program "android-studio"
        :classes '("jetbrains-studio" "Spring Tool Suite" "Eclipse")
        :cmd eclipse-cmd
        :raise-key "H-r")))

(define-run-or-pull-program "zathura")

(define-run-or-pull-program "virtualbox"
  :classes '("VirtualBox Machine" "VirtualBox Manager")
        :cmd "virtualbox"
        :raise-key "H-v")

(define-run-or-pull-program "pavucontrol"
  :classes '("Pavucontrol" )
  :cmd "pavucontrol"
  :pull-key "H-o"
  :raise-key "H-O")

(defvar *games-keymap* (make-sparse-keymap))

(define-run-or-pull-program "eboard"
  :raise-key "H-e"
  :pull-key "H-E"
  :classes '("eboard" "Eboard")
  :keymap *games-keymap*)

(define-run-or-pull-program "signal-desktop"
  :raise-key "H-s"
  :pull-key "H-S"
  :classes '("signal" "Signal"))

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
	    (define-key kmap kbd action))))


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
      ;;("H-v" "raise-vbox")
      ;;("H-p" "raise-pidgin")
      ("Scroll_Lock" "fnext")
      ("H-a" "echo-date-battery")
      ("H-w" "echo-windows-with-group")

      ("H-k" "delete")
      ("H-K" "kill")
      ("H-;" "colon")
      ("H-:" "eval")
      ("H-x" "exec")
      ("H-X" *commands-map*)
      ;; ("H-z" *commands-map*)
      ("H-u" *utils-map*)
      ("F12" "run-shell-command find-cursor")
      ("H-F12" "scrot-cmd-current-window-anon")

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
      ("XF86AudioLowerVolume" "vol-down")
      ("XF86AudioRaiseVolume" "vol-up")

      ("H-F8" "vol-mute-toggle")
      ("H-F9" "vol-down")
      ("H-F10" "vol-up")
      ("H-SPC" "speak-key")
      ("H-t" *text-shortcuts-map*)

      ("H-N" "gnext")
      ("H-P" "gprev")
      ("H-d" *games-keymap*)
      ("H-B" "sip-call-dtmf")
      ("H-b" "sip-main")
      ("H-V" "paste-primary")
      ("H-T" "openproject-create-personal-task")
      ("H-q" "run-last-kbd-macro")
      ("H-Q" "run-kbd-macro")
      (,(make-key :keysym 65258) "middle-click")
      (,(make-key :keysym 65535) "middle-click")
      ("H-r" "redshift-shift-red")
      ("H-R" "redshift-reset"))
  ;;not efficient nor necessary but only run at initialization
  )



(defcommand gselect-nth (i) ((:number "enter group number: "))
  (assert (< i (length (sort-groups (current-screen)))))
  '(switch-to-group group-name)
  (-> i
      (nth (sort-groups (current-screen)))
      switch-to-group))

(loop
   for group-name in '("Default" "F2" "F3" "F4" "F5")
   for i from 0
   as key = (format nil "H-F~D" (1+ i))
   as command = (format nil "gselect-nth ~D" i)
   do (gnewbg group-name)
   do
     (define-key-bindings (all-top-maps)
         (list (list key command))))

(define-key-bindings
    *correct-screen-map*
    `(
      ("H-p" "correct-screen-prompt-display-order")
      ("H-a" "correct-screen-all-connected-displays")
      ("H-n" "correct-screen-newly-connected-displays")
      ("H-1" "correct-screen-only-current-display")
      ("H-m" "correct-screen-select-mode")))

(define-key-bindings
 *utils-map*
 '(
   ("i" "invert-screen")
   ("M" "echo-pointer")
   ("m" "toggle-magnifier")
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
   ("k" "speak-string")
   ("l" "spell-clipboard")
   ("H-l" "screen-lock")
   ("l" "screen-lock")
   ;; ("H-e" "echo-current-tab" )
   ("H-e" "emacs-killusr2")
   ("H-E" "emacs-killusr2-tmux")
   ("z" "window-sleep-toggle")
   ("H-t" "tmp")
   ("H-b" "byzanz-record-auto")
   ("H-B" "byzanz-record-auto-stop")
   ("H-s" "toggle-screen-key")
   ("b" *brightness-map*)
   ("H-p" "center-pointer")
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

(set-prefix-key (kbd "F19"))
(pop-top-map)
;; TODO use buttons framework
