(loop while t
   as stream = (usocket:socket-stream
                (usocket:socket-accept
                 (usocket:socket-listen "localhost" 1235)))
   as line = (read-line stream)
   do (format t "line is: ~A" line))
