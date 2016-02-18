(defcommand search-engine-search-clipboard ()
    ()
    (search-engine-search "ddg" (get-x-selection )))

(defcommand type-string (s)
  ((:string ))
  ;;(echo-format "times is ~D" times)
  ;;(sleep .001)
  (xdotool (concat "type " s))
  )


;;(setq *commands-map* (make-sparse-keymap))
(setq *snippets-map* (make-sparse-keymap))

(define-key-bindings
  *commands-map*
  '(
   ("g" "search-engine-search-clipboard")
   ("i" *snippets-map*)
   ("y" "youtube-wget")
   )
  )

;;TODO identify pressed keys and release them
(define-key-bindings
  *snippets-map*
  '(
   ("@" "type-string erjoalgo@gmail.com")
   ("6" "type-string 162.228.201.6")
   )
  )
