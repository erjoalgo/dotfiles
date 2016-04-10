(setf *per-window-bindings-rules*
      `(
  ;;(("Iceweasel" "Navigator" "Firefox" www-browser "Chromium")
  (,stumpwm::*browser-classes*
   ("F2" (send-fake-key (current-window) (kbd "C-TAB")))
   ("F1" (send-fake-key (current-window) (kbd "C-ISO_Left_Tab")))
   ("F4" (send-fake-key (current-window) (kbd "C-F4")))
   ("S-F4" (send-fake-key (current-window) (kbd "C-T")))
   ("F3" (send-fake-key (current-window) (kbd "C-g")) )
   ;;("S-F4" (send-fake-key (current-window) (kbd "C-S-F4")))
   ("F9" (send-fake-key (current-window) (kbd "C--")))
   ("F10" (send-fake-key (current-window) (kbd "C-=")))
   ("M-F1" (send-fake-key (current-window) (kbd "M-Left")))
   ("M-F2" (send-fake-key (current-window) (kbd "M-Right")))

   ("S-F1" (send-fake-key (current-window) (kbd "C-S-SunPageUp")))
   ("S-F2" (send-fake-key (current-window) (kbd "C-S-SunPageDown")))

   ;;("d" (send-fake-key (current-window) (kbd "SunPageDown")))
   
   ;;("w" (send-fake-key (current-window) (kbd "SunPageUp")))
   ;;("a" (send-fake-key (current-window) (kbd "Left")))
   ;;("s" (send-fake-key (current-window) (kbd "Right")))
   
   )

  (("x-terminal-emulator" "X-terminal-emulator" "roxterm" "Roxterm")
   ("SunPageUp" (send-fake-key (current-window) (kbd "S-SunPageUp")) (echo "sunpageup"))
   ("SunPageDown" (send-fake-key (current-window) (kbd "S-SunPageDown")) (echo "sunpagedown"))
					;("F1" (send-fake-key (current-window) (kbd "C-ISO_Left_Tab")))
					;("F4" (send-fake-key (current-window) (kbd "C-F4")))
					;("M-F1" (send-fake-key (current-window) (kbd "M-Left")))
   )

  (("Evince" "evince")
   ;;("d" (send-fake-key (current-window) (kbd "Down")))
   ;;("w" (send-fake-key (current-window) (kbd "Up")))
   ;;("a" (send-fake-key (current-window) (kbd "Left")))
   ;;("s" (send-fake-key (current-window) (kbd "Right")))
   ;;("g" (send-fake-key (current-window) (kbd "C-l")))
   ;;("n" (send-fake-key (current-window) (kbd "C-SunPageDown")))
   ;;("p" (send-fake-key (current-window) (kbd "C-SunPageUp")))
   ("F3" (send-fake-key (current-window) (kbd "C-f")))
   ("-" (send-fake-key (current-window) (kbd "C--")))
   ("+" (send-fake-key (current-window) (kbd "C-+")))
   )
  ;;(("Zathura" "zathura")
  
  ;;("SunPageUp" (echo "hola4") (send-fake-key (current-window) (kbd "S-SunPageUp")) (run-shell-command "xdotool key shift+Prior" t) )
  ;;("SunPageDown" (send-fake-key (current-window) (kbd "S-SunPageUp")) (run-shell-command "xdotool key shift+Next"))
  ;;("F1" (send-fake-key (current-window) (kbd "C-ISO_Left_Tab")))
  ;;("F4" (send-fake-key (current-window) (kbd "C-F4")))
  ))
