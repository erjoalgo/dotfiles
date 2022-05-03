(asdf:defsystem
    :erjoalgo-stumpwmrc
  :serial t
  :description "personal stumpwm config"
  :license "GPLv3"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :depends-on
  (
   #:stumpwm ;; this should not be loaded from quicklisp, but from the latest sources
   #:usocket ;; mozrepl, swank, etc
   #:cl-csv  ;; contacts
   #:swank ;; swank-loader
   #:hunchentoot ;; x-service
   #:babel ;; x-service
   #:clx ;; xlib references
   #:websocket-driver-client ;; sms-fanout
   #:cl-json ;; sms-fanout
   #:cl-who ;; contacts html rendering
   #:cl-syslog ;; sms-fanout
   #:cladaver ;; url launcher, etc
   #:statusor
   #:access
   #:drakma ;; TODO factor-out openproject.lisp
   #:lparallel
   #:erjoalgo-webutil
   )
  :components
  (
   ;; (:file "packages")
   (:file "commands/selcand")
   (:file "util")
   (:file "defs")
   (:file "xinitrc")
   (:file "decorations")
   (:file "pid-util")
   (:file "mozrepl")
   (:file "defcommands")
   (:file "psym")
   (:file "authinfo")
   (:file "search-engine-search")
   (:file "search-engines")
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
   (:file "commands/contacts")
   (:file "commands/voipms-helper")
   (:file "commands/sip")
   (:file "commands/speech")
   (:file "commands/vol")
   (:file "commands/sms-fanout-client")
   (:file "commands/snipit")
   (:file "commands/openproject")
   (:file "commands/middle-click")
   (:file "init")))
