;(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
;(ql:quickload :swank)
;;TODO get swank to work with slime 2.17

(push (merge-pathnames "quicklisp/dists/quicklisp/software/slime-v2.17/")
      asdf:*central-registry*)
(require :swank)

(defvar *swank-port* 4005)
(defcommand swank () ()
  "start swank server on *swank-port*"
  (echo-string (current-screen) 
	       "Starting swank...")
  (swank:create-server :port *swank-port*
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen) 
	       "Started swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

(defcommand swank-stop () ()
  "stop swank server"
  (echo "stopping swank")
  (swank:stop-server 4005)
  (echo "stopped swank"))

(swank)


;; (require 'swank)
;; (swank:create-server :port 4005 :style swank:*communication-style* :dont-close t)
