;(defvar *top-hash-map* nil )

;;to avoid sync-keys on every define-key
(defvar *real-top-map* nil)
(setf *real-top-map* *top-map*)

;; why are we pushing a real copy of top map, which will be discarded?
;; perhaps to leave us with a usabe top-map if eval of this file fails
;; before (pop-top-map)
(push-top-map (make-sparse-keymap))

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

      ("H-/" "pull-hidden-other")
      ("H--" "fclear")
      ("H-TAB" "fnext")

      ("H-h" *help-map*)

      ("H-d" "dict-lookup-command")

      ("H-DEL" *special-chars-map*)
      ("H-I" "invert-current-window")
      ("H-i" "invert-screen")

      ("H-p" "pull-hidden-previous")
      ("H-n" "pull-hidden-next")

      ;; ("H-g" "search-engine-search baidu")
      ("H-g" "search-engine-search ddg")

      ("H-m" *search-engine-map*)
      ("H-D" "cat-message-command ~/vocab")
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

      ("H-F8" "volmute-toggle")
      ("H-F9" "voldown")
      ("H-F10" "volup")
      ("H-SPC" "speak-key")
      )

  ;;not efficient nor necessary but only run at initialization
  )



(define-key-bindings (all-top-maps)
    (append
     (loop for i from 2 upto 6
	do (gnewbg (format nil "F~D" i))
	nconc
	  `((,(format nil "H-F~D" i) ,(format nil "gselect ~D" i))
	    ;; (,(format nil "S-H-F~D" i) ,(format nil "gmove ~D" i))))
            ))

     '(("H-F1" "gselect Default")
       ("S-H-F1" "gmove Default"))))


(define-key-bindings
 *utils-map*
 '(
   ("i" "invert-screen")
   ;;("k" "run-shell-command call_skype_numer.py -w")
   ("M" "echo_pointer")
   ("m" "toggle-magnifier")
   ("r" *screen-rotation-map*)
   ("c" *special-characters-map*)
   ("w" "connect-internet")
   ("h" *help-map*)
   ("s" "scrot-cmd" )
   ("S" "scrot-cmd-full-screen" )
   ("o" "ocr-scrot-clipboard" )
   ("k" "speak-string" )
   ("K" "spell-clipboard" )
   ))


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

(define-key *help-map* (kbd "g") "echo-current-group-name")
;;(define-key *special-characters-map* (kbd "?") "run-shell-command xdotool key ¿")

(pop-top-map)
(set-prefix-key (kbd "F19"))


(reload-search-engines)
