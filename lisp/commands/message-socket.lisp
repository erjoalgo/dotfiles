(defvar *port-socket-alist*
  nil
  "An alist to keep a track of usocket servers by port.")

(defun usocket-server (port &key shutdown-if-exists
                              (host "localhost"))
  "Return a socket listening on the given PORT.
   If a socket already exists, is not closed, and
   SHUTDOWN-IF-EXISTS is non-nil, it is returned.
   Otherwise, any existing socket listening on the port
   is closed and a new socket is created"

  (let* ((entry (assoc port *port-socket-alist*))
        (server (cdr entry)))
    (unless entry
      (setf entry (cons port nil))
      (push entry *port-socket-alist*))
    (if (and server
             (not shutdown-if-exists))
        server
        (progn
          (when server (usocket:socket-close server))
          (setf (cdr entry)
                (usocket:socket-listen host port :reuse-address t))))))

(defun message-socket-listen (fn &key port)
  "Start a usocket server continuously listening
   for incoming connections on PORT.
   For each connection, read stream until EOF
   and invoke FN with a list of lines read."

  (loop with server = (usocket-server port)
        as conn = (progn
                    ;; (format t "got a connection!~%")
                      (usocket:socket-accept server))
        as stream = (usocket:socket-stream conn)
        as lines = (loop as line = (read-line stream nil :eof)
                         while (not (eq :eof line))
                         collect line)
        ;; do (format t "read ~D lines~%" (length lines))
        do (funcall fn lines)))


(defun messages-listen (&key (port 1959))
  "Continuously listen on a socket for incoming messages and echo them visually."
  (message-socket-listen (lambda (lines)
                           (echo-string-list (current-screen) lines))
                         :port port))
