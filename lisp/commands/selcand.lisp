(defpackage :selcand
  (:use :cl)
  (:export
   #:select))
(in-package :selcand)

(defvar *default-hints*
  "1234acdefqrstvwxz"
  "Default hint chars.")

(defun hints (candidates &optional chars)
  "Return an alist (HINT . CAND) for each candidate in CANDIDATES.

  Each hint consists of characters in the string CHARS."
  (setf chars (or chars *default-hints*))
  (assert candidates)
  (loop
    with hint-width = (ceiling (log (length candidates) (length chars)))
    with hints = '("")
    for wi below hint-width do
      (setf hints
            (loop for c across chars
                  append (mapcar (lambda (rest)
                                   (format nil "~C~A" c rest))
                                 hints)))
    finally (return (loop for hint in hints
                          for cand in candidates
                          collect (cons hint cand)))))

(defun select (&key
                 candidates
                 hints-candidates
                 (prompt "select candidate: ")
                 (stringify-fn #'prin1-to-string)
                 (autoselect-if-single t)
                 (on-empty-error "No candidates available!")
                 no-hints
                 display-candidates
                 read-char-if-possible
                 initial-candidate-index
                 columns)
  "Use PROMPT to prompt for a selection from CANDIDATES."
  (assert (or candidates hints-candidates) nil on-empty-error)
  (assert (alexandria:xor candidates hints-candidates))
  (let* ((hints-cands
           (cond
             (hints-candidates hints-candidates)
             (no-hints
              (mapcar
               (lambda (cand)
                 (cons (stumpwm::string-trim " " (funcall stringify-fn cand)) cand))
               candidates))
             (t
              (loop for (hint . cand) in (hints candidates)
                    collect
                    (cons
                     (stumpwm::string-trim
                      " "
                      (concatenate
                       'string hint
                       ") " (funcall stringify-fn cand)))
                     cand)))))
         (choices (mapcar #'car hints-cands))
         (prompt (cond
                   ((not display-candidates) prompt)
                   ((null columns)
                    (format nil "~A~%~{~A~^~%~}~%~% "
                            prompt
                            (if (eq display-candidates :include-values)
                                hints-cands
                                choices)))
                   (columns
                    (format nil "~{~A~^~%~}"
                            (stumpwm::columnize (cons prompt
                                                      (if (eq display-candidates :include-values)
                                                          hints-cands
                                                          choices))
                                                columns)))))
         (hint-selected
           (cond
             ((and autoselect-if-single (null (cdr choices))) (car choices))
             ((and read-char-if-possible
                   (eq 1 (reduce #'max (mapcar #'length choices))))
              (loop
                for i below 3
                as choice = (progn
                              (stumpwm:message "~D ~A" i prompt)
                              (format nil "~C"
                                      (or
                                       (stumpwm:read-one-char
                                        (stumpwm:current-screen))
                                       (throw 'error "Abort."))))
                while (not (member choice choices :test #'equal))
                finally
                   (progn
                     (stumpwm::unmap-all-message-windows)
                     (return choice))))
             (t (or (stumpwm:completing-read
                     (stumpwm:current-screen) prompt choices
                     :initial-input (when initial-candidate-index
                                      (nth initial-candidate-index choices)))
                    (throw 'error "Abort.")))))
         (cand (cdr (assoc hint-selected hints-cands :test #'equal))))
    (assert (or (null hint-selected) cand))
    cand))
