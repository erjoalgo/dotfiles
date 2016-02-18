(defun define-key-bindings (kmap bindings &optional more-kmaps)
  (when kmap (setq more-kmaps (cons kmap more-kmaps)))
  (mapc (lambda (kmap)
	  (mapc (lambda (kbd-action)
		  (destructuring-bind (key action) kbd-action
		      (define-key kmap (kbd key) action)))
		bindings))
	more-kmaps)
  )


(mapcar (lambda (kmap-symbol) (set kmap-symbol (make-sparse-keymap)))
	'(
	  *screen-rotation-map*
	  *utils-map*
	  *special-chars-map*
	  *commands-map*
	  ;;*help-map*
	  ))

(define-key-bindings
  *top-map*
  `(
    ("XF86PowerOff" "echo power pressed root!")
    ;;volume
    ("KP_Multiply" "run-shell-command volup")
    ("XF86AudioLowerVolume" "run-shell-command voldown")
    ("XF86AudioRaiseVolume" "run-shell-command volup")
    ("H-F9" "voldown")
    ("H-F10" "volup")
    ;;("XF86AudioLowerVolume" nil)
    ;;("XF86AudioRaiseVolume" nil)

    ("s-n" "scroll-browser-other-frame Down")
    ("s-p" "scroll-browser-other-frame Up")
    ("s-n" "scroll-browser-other-frame d")
    ("s-p" "scroll-browser-other-frame w")
    ;;("s-F4" "scroll-browser-other-frame C-F4")

    ("Hiragana_Katakana" "run-shell-command /home/ernesto/.xmodmap_jap")
    ("H-e" "my-emacs-cmd")
    ;;("H-E" "pull-emacs")
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
    ("H-G" "search-engine-search goog")
    ("H-d" "dict-lookup-command")

    ;;("H-DEL" "run-shell-command xdotool key Hyper_L 0 type '`'")
    ("H-DEL" *special-chars-map*)
    ("H-P" "flush-and-message-debug")
    ("H-I" "invert-current-window")
    ("H-i" "invert-screen")
    ;;("H-]" "go-next")
    ;;("H-[" "go-prev")
    ("H-p" "pull-hidden-previous")
    ("H-n" "pull-hidden-next")
    ;;("XF86AudioMute" "run-shell-command bash -c 'scrot && notify-send scrot taken || notify-send scrot failed'")
    ("H-Up" "run-shell-command brightnesschange.py u")
    ("H-Down" "run-shell-command brightnesschange.py d")
    ("H-g" "search-engine-search ddg")
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
  (list-from-hash *top-hash-map* (lambda (k v) v))
  ;;not efficient nor necessary but only run at initialization
 )

(defun all-top-maps ()
  (cons *top-map*
	(list-from-hash *top-hash-map* (lambda (k v) v)))
  )

(define-key-bindings nil 
    (append 
     (apply 'append (loop for i from 2 upto 6 
		do (gnewbg (format nil "F~D" i))
		collect
		  (list
		   (list (format nil "H-F~D" i) (format nil "gselect ~D" i))
		   (list (format nil "S-H-F~D" i) (format nil "gmove ~D" i)))
		  ))
     (list
      (list "H-F1" "gselect Default")
      (list "S-H-F1" "gmove Default"))
     )
  (all-top-maps)
  )


(define-key-bindings
 *utils-map*
 '(
   ("i" "invert-screen")
   ("t" "run-shell-command take_pic.py -xb > /home/ernesto/tpout 2>&1")
   ("P" "run-shell-command echo $PATH -xb >> /home/ernesto/tpout 2>&1")
   ("P" "run-shell-command echo $PATH -xb >> /home/ernesto/tpout 2>&1")
   ("C" "run-shell-command correct_screen.py &")
   ("k" "run-shell-command call_skype_numer.py -w")
   ("p" "run-shell-command save_pics.py")
   
   ("s" "save_page")
   ("p" "run-shell-command pgkill.py save_pic -f")
   ("T" "kill-tk-windows")
   ("M" "echo_pointer")
   ("m" "toggle-magnifier")
					;("f" "scrot_fema")
   ("F" "define-button")
   ("P" "save_pic")
   ("r" *screen-rotation-map*)
   ("c" *special-characters-map*)
   ("w" "connect-internet")
   ))


(define-key-bindings *screen-rotation-map*
 '(
   ("a" "rotate-screen left")
   ("d" "rotate-screen right")
   ("w" "rotate-screen normal")))

(define-key-bindings
    *special-chars-map*
    `(
      ("DEL" "insert-key-with-delay grave")
      ("3" "insert-key-with-delay grave 3")
      ("?" "insert-key-with-delay questiondown")
      ("u" "insert-key-with-delay udiaeresis")
      ("n" "insert-key-with-delay ntilde")
      ("!" "run-shell-command xdotool key exclamdown")
      ("~" "insert-key-with-delay degree")
      ("$" "insert-key-with-delay EuroSign")
      )
  )


(define-key *help-map* (kbd "g") "echo-current-group-name")
;;(define-key *special-characters-map* (kbd "?") "run-shell-command xdotool key ¿")

(set-prefix-key (kbd "SunPrint_Screen"))

'(loop for map in (all-top-maps)
      do (define-key map (kbd "s-F4") nil ))
	   
