(in-package :STUMPWM)

(defmacro case-string (keyform &body cases)
  `(cond ,@(loop for (case-key . case-body) in cases
                 collect `((string= ,keyform ,case-key)
	                   ,@case-body))))


(defparameter process-state-codes-alist
  '((:D . :UNINTERRUPTIBLE);    uninterruptible sleep (usually IO);
    (:R . :RUNNING);    running or runnable (on run queue);
    (:S . :SLEEP);    interruptible sleep (waiting for an event to complete);
    (:T . :STOPPED);    stopped, either by a job control signal or because it is being traced
    (:W . :PAGING);    paging (not valid since the 2.6.xx kernel);
    (:X . :DEAD);    dead (should never be seen);
    (:Z . :DEFUNCT);    defunct ("zombie"); process, terminated but not reaped by  its parent
    (:|t| . :DEBBUGGER);    (lowe-case t) stopped by debugger during the tracing
    )
  "map process codes as returned by 'ps -o state=' to a keyword")


(defun process-state (pid)
  (let* ((cmd (format nil "ps -p ~D -o state=" pid))
	 (proc-state-string (run-shell-command cmd t))
	 (trimmed (string-trim '(#\newline) proc-state-string))
	 (kw (intern trimmed "KEYWORD")))
    ;;(format t "cmd ~A, kw ~A, out: ~A ~A~%" cmd proc-state-string trimmed kw)
    (or (cdr (assoc kw process-state-codes-alist))
	(error "unknown state ~A" proc-state-string))))

(defun kill-process (pid signal)
  (let ((cmd (format nil "kill -~A ~D" signal pid)))
    (message cmd)
    (run-shell-command cmd)))

(format t "loaded pid-util.lisp")
