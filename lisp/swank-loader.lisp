(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload :swank)
(defcommand swank () ()
  (echo-string (current-screen) 
	       "Starting swank...")
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen) 
	       "Started swank. M-x slime-connect RET RET, then (in-package stumpwm)."))

(defcommand swank-stop () ()
  (echo "stopping swank")
  (swank:stop-server 4005)
  (echo "stopped swank")
  )

(swank)


;; (require 'swank)
;; (swank:create-server :port 4005 :style swank:*communication-style* :dont-close t)
