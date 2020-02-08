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

(defun select (candidates &optional prompt stringify-fn autoselect-if-single
                            no-hints)
  "Use PROMPT to prompt for a selection from CANDIDATES."
  (let* ((sep ") ")
         (stringify-fn (or stringify-fn #'prin1-to-string))
         (hints-cands
          (if no-hints
              (mapcar
               (lambda (cand)
                 (cons (stumpwm::string-trim " " (funcall stringify-fn cand)) cand))
               candidates)
              (loop for (hint . cand) in (hints candidates)
                 collect (cons
                          (stumpwm::string-trim " "
                           (concatenate 'string hint
                                        sep (funcall stringify-fn cand)))
                          cand))))
         (choices (mapcar #'car hints-cands))
         (prompt (or prompt "select candidate: "))
         (hint-selected (if (and autoselect-if-single (null (cdr choices)))
                            (car choices)
                            (or (stumpwm:completing-read
                                 (stumpwm:current-screen) prompt choices)
                                (throw 'error "Abort."))))
         (cand (cdr (assoc hint-selected hints-cands :test #'equal))))
    (assert (or (null hint-selected) cand))
    cand))
