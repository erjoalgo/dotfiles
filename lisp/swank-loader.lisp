(in-package :STUMPWM)

(push (merge-pathnames "quicklisp/dists/quicklisp/software/slime-v2.17/")
      asdf:*central-registry*)

(require :swank)

(defun socket-listening-p (host port)
  (handler-case (usocket:socket-connect host port)
    (USOCKET:CONNECTION-REFUSED-ERROR (ex)
      (declare (ignore ex)) nil)))

(defun swank-start (&key (port 4005))
  "start swank server on *swank-port*"
  (if (socket-listening-p "127.0.0.1" port)
      (warn "already listening on ~A. skipping swank..." port)
      (progn
        (echo-string (current-screen)
	             "Starting swank...")
        (swank:create-server :port port
                             :style swank:*communication-style*
                             :dont-close t)
        (echo-string (current-screen)
	             "Started swank. M-x slime-connect."))))

(defcommand swank () ()
  (swank-start))

(defcommand swank-stop () ()
  "stop swank server"
  (echo "stopping swank")
  (swank:stop-server 4005)
  (echo "stopped swank"))

