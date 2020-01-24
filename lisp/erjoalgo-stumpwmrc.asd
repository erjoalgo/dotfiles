(asdf:defsystem
 :erjoalgo-stumpwmrc
 :serial t
 :description "personal stumpwm config"
 :license "GPLv3"
 :author "Ernesto Alfonso <erjoalgo@gmail.com>"
 :depends-on
 (#:stumpwm
  #:usocket ;; mozrepl, swank, etc
  #:cl-csv  ;; contacts
  #:swank ;; swank-loader
  #:hunchentoot ;; x-service
  #:babel ;; x-service
  #:clx ;; xlib references
  #:websocket-driver-client ;; sms-fanout
  #:cl-json ;; sms-fanout
  )
 :components
 (
  ;; (:file "packages")
  (:file "util")
  (:file "defs")
  (:file "defs-secret")
  (:file "xinitrc")
  (:file "decorations")
  (:file "pid-util")
  (:file "defcommands")
  (:file "mozrepl")
  (:file "psym")
  (:file "url-launcher")
  (:file "text-shortcuts")
  (:file "startup-apps")
  (:file "run-raise-programs")
  (:file "per-window-bindings")
  ;; (:file "top-map-bindings") TODO too many side-effects. loaded dynamically
  (:file "swank-loader")
  (:file "correct-screen")
  (:file "brightness")
  (:file "x-service")
  (:file "commands/scrot")
  (:file "commands/battery-notification")
  (:file "commands/selcand")
  (:file "commands/contacts")
  (:file "commands/sip")
  (:file "commands/speech")
  (:file "commands/vol")
  (:file "commands/sms-fanout-client")))