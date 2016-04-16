;(defvar *top-hash-map* nil )

;;to avoid sync-keys on every define-key
(defvar *real-top-map* nil )
(setf *real-top-map* *top-map*)
(push-top-map (deep-copy-map *top-map*))

(defun define-key-bindings (kmap-or-kmap-list bindings)
  (loop with kmap-list = (if (listp kmap-or-kmap-list)
			     kmap-or-kmap-list
			     (list kmap-or-kmap-list))
     for kmap in kmap-list do
       (loop for (key action) in bindings do
	    (define-key kmap (kbd key) action))))


(defmacro def-several-vars (value-form &rest vars)
  `(progn ,@(loop for var in vars collect `(defvar ,var ,value-form))))

(def-several-vars
  (make-sparse-keymap)
  *screen-rotation-map*
  *utils-map*
  *special-chars-map*
  *search-engine-map*
  *commands-map*)

(defun all-top-maps ()
  (append (list *real-top-map*)
	  (when *per-window-bindings-class-to-map*
	    (loop for map being the hash-value of *per-window-bindings-class-to-map*
	       collect map))))

(define-key-bindings
    (all-top-maps)
    `(
      ("XF86PowerOff" "echo power pressed root!")
      ;;volume
      ("KP_Multiply" "run-shell-command volup")
      ("XF86AudioLowerVolume" "voldown")
      ("XF86AudioRaiseVolume" "volup")
      ("H-F9" "voldown")
      ("H-F10" "volup")

      ;;brightness
      ;;emergency dimming 
      ("XF86MonBrightnessDown" "run-shell-command xbacklight -set 1")
      ("XF86MonBrightnessUp" "run-shell-command xbacklight -set 50")
      ;;("XF86AudioLowerVolume" nil)
      ;;("XF86AudioRaiseVolume" nil)

      ("H-P" "scroll-browser-other-frame w")
      ("H-N" "scroll-browser-other-frame d")
					;("s-p" "scroll-browser-other-frame w")
					;("s-n" "scroll-browser-other-frame d")
      ;;("s-F4" "scroll-browser-other-frame C-F4")

      ("Hiragana_Katakana" "run-shell-command /home/ernesto/.xmodmap_jap")
      ;;("H-e" "my-emacs-cmd")
      ("H-e" "emacs")
      ("H-E" "pull-emacs")
      ;;("H-f" "raise-iceweasel")
      ;;("H-c" "raise-x-terminal-emulator")
      ;;("H-m" "raise-remmina")
      ;;("H-m" "raise-matlab")
      ;;("H-v" "raise-vbox")
      ;;("H-E" "pull-emacs")
      ;;("H-F" "pull-iceweasel")
      ;;("H-C" "pull-x-terminal-emulator")
      ;;("H-V" "pull-vbox")
      ;;("H-p" "raise-pidgin")
      ("Scroll_Lock" "fnext")
      ;;("Pause" "run-shell-command save_pics.py")
      ;;("Pause" "pull-hidden-other")
      ;;("Pause" nil)
      ;;("s-F1" "emacs")
      ;;("s-F2" "raise-iceweasel")
      ;;("s-F3" "raise-x-terminal-emulator")
      ;;("s-F4" "raise-remmina")
      ;;("H-M" "pull-remmina")
      ("H-a" "echo-date-battery")
      ;;("H-w" "windows")
      ("H-w" "my-echo-windows")

      ("H-k" "delete")
      ("H-K" "kill")
      ("H-;" "colon")
      ("H-:" "eval")
      ("H-x" "exec")
      ("H-X" *commands-map*)
      ("H-z" *commands-map*)
      ("H-u" *utils-map*)

      ("H-/" "pull-hidden-other")
      ("H--" "fclear")
      ("H-TAB" "fnext")

      ;;("H-q" "only")
      ;;("H-3" "hsplit")
      ;;("H-2" "vsplit")
      ;;("H-#" "hsplit")
      ;;("H-@" "vsplit")
      ;;("H-q" "vsplit")
      ("H-h" *help-map*)

      ;;("H-G" "search-engine-search-none")
      ("H-G" "search-engine-search")
      ("H-d" "dict-lookup-command")

      ;;("H-DEL" "run-shell-command xdotool key Hyper_L 0 type '`'")
      ("H-DEL" *special-chars-map*)
      ;;("H-P" "flush-and-message-debug")
      ("H-I" "invert-current-window")
      ("H-i" "invert-screen")
      ;;("H-]" "go-next")
      ;;("H-[" "go-prev")
      ("H-p" "pull-hidden-previous")
      ("H-n" "pull-hidden-next")
      ;;("XF86AudioMute" "run-shell-command bash -c 'scrot && notify-send scrot taken || notify-send scrot failed'")
					;("H-Up" "run-shell-command brightnesschange.py w")
					;("H-Down" "run-shell-command brightnesschange.py d")
      ("H-g" "search-engine-search ddg")
					;("H-m" "search-engine-search music")
      ("H-m" "search-engine-search mulator")
      ;;("H-m" "raise-remmina")
      ;;("H-m" "raise-matlab")
      ;;("H-v" "raise-vbox")
      ;;("H-E" "pull-emacs")
      ;;("H-F" "pull-iceweasel")
      ;;("H-C" "pull-x-terminal-emulator")
      ;;("H-V" "pull-vbox")
      ;;("H-p" "raise-pidgin")
      ("Scroll_Lock" "fnext")
      ;;("Pause" "run-shell-command save_pics.py")
      ;;("Pause" "pull-hidden-other")
      ;;("Pause" nil)
      ;;("s-F1" "emacs")
      ;;("s-F2" "raise-iceweasel")
      ;;("s-F3" "raise-x-terminal-emulator")
      ;;("s-F4" "raise-remmina")
      ;;("H-M" "pull-remmina")
      ("H-a" "echo-date-battery")
      ;;("H-w" "windows")
      ("H-w" "my-echo-windows")

      ("H-k" "delete")
      ("H-K" "kill")
      ("H-;" "colon")
      ("H-:" "eval")
      ("H-x" "exec")
      ("H-X" *commands-map*)
      ("H-z" *commands-map*)
      ("H-u" *utils-map*)

      ("H-/" "pull-hidden-other")
      ("H--" "fclear")
      ("H-TAB" "fnext")

      ;;("H-q" "only")
      ;;("H-3" "hsplit")
      ;;("H-2" "vsplit")
      ;;("H-#" "hsplit")
      ;;("H-@" "vsplit")
      ;;("H-q" "vsplit")
      ("H-h" *help-map*)

      ;;("H-G" "search-engine-search-none")
      ("H-G" "search-engine-search")
      ("H-d" "dict-lookup-command")

      ;;("H-DEL" "run-shell-command xdotool key Hyper_L 0 type '`'")
      ("H-DEL" *special-chars-map*)
      ;;("H-P" "flush-and-message-debug")
      ("H-I" "invert-current-window")
      ("H-i" "invert-screen")
      ;;("H-]" "go-next")
      ;;("H-[" "go-prev")
      ("H-p" "pull-hidden-previous")
      ("H-n" "pull-hidden-next")
      ;;("XF86AudioMute" "run-shell-command bash -c 'scrot && notify-send scrot taken || notify-send scrot failed'")
					;("H-Up" "run-shell-command brightnesschange.py w")
					;("H-Down" "run-shell-command brightnesschange.py d")
      ("H-g" "search-engine-search ddg")
					;("H-m" "search-engine-search music")
      ("H-m" *search-engine-map*)
      ;;("H-G" "search-engine-search wiki")
      ("H-D" "cat-message-command ~/vocab")
      ("H-l" "launch-url")
      ("H-L" "launcher-append-url")
      ("H-Home" "asdfasdf")
      ;;("H-e" "emacs")
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
      
      ("H-F8" "volmute-toggle")
      ("H-F9" "voldown")
      ("H-F10" "volup")
      )
  
  ;;not efficient nor necessary but only run at initialization
  )



(define-key-bindings (all-top-maps) 
    (append
     (loop for i from 2 upto 6 
	do (gnewbg (format nil "F~D" i))
	nconc
	  `((,(format nil "H-F~D" i) ,(format nil "gselect ~D" i))
	    (,(format nil "S-H-F~D" i) ,(format nil "gmove ~D" i))))
     
     '(("H-F1" "gselect Default")
       ("S-H-F1" "gmove Default"))))


(define-key-bindings
 *utils-map*
 '(
   ("i" "invert-screen")
   ("C" "run-shell-command correct_screen.py &")
   ;;("k" "run-shell-command call_skype_numer.py -w")
   ("p" "run-shell-command save_pics.py")
   
   ("s" "save_page")
   ("p" "run-shell-command pgkill.py save_pic -f")
   ("T" "kill-tk-windows")
   ("M" "echo_pointer")
   ("m" "toggle-magnifier")
   ("F" "define-button")
   ("P" "save_pic")
   ("r" *screen-rotation-map*)
   ("c" *special-characters-map*)
   ("w" "connect-internet")
   ("h" *help-map*)))


(define-key-bindings *screen-rotation-map*
 '(
   ("a" "rotate-screen left")
   ("d" "rotate-screen right")
   ("w" "rotate-screen normal")))

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
    *search-engine-map*
    `(
      ("g" "search-engine-search ddg")
      ("m" "search-engine-search music")
      ("i" "search-engine-search bingimg")
      ("w" "search-engine-search wordreference")
      ("y" "search-engine-search youtube")
      ("s" "search-engine-search stackoverflow")
      ("w" "search-engine-search wiki")
      ))


(define-key *help-map* (kbd "g") "echo-current-group-name")
;;(define-key *special-characters-map* (kbd "?") "run-shell-command xdotool key ¿")

(pop-top-map)
(set-prefix-key (kbd "SunPrint_Screen"))
	   

