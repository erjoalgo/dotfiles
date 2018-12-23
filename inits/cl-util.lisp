(in-package #:CL-USER)

(defun describe-obj (obj)
  (loop with class = (class-of obj)
     for slot in (sb-mop:class-slots class)
     as slot-name = (slot-value slot 'sb-pcl::name)
     as slot-value = (if (SLOT-BOUNDP obj slot-name)
                         (slot-value obj slot-name)
                         :UNBOUND)
     collect (list slot-name slot-value)))

(defun describe-hash-table (table)
  '(with-hash-table-iterator (itr table)
    (loop
       (multiple-value-bind (entry-p key value)
           (itr)
         (if entry-p
             (print-hash-entry key value)
             (return)))))
  (loop for k being the hash-keys of table
      collect (cons k (gethash k table))))

(defvar dbg nil)

(defun package-unintern-shadowing-symbols (&optional pkg)
  (setf pkg (or pkg *package*))
  (let ((syms (package-shadowing-symbols pkg)))
    (dolist (sym syms) (unintern sym pkg))
    (format t "uninterned ~D shadowing symbols ~%"
            (length syms))))

(defun package-unexport-all-symbols (&optional pkg)
  (setf pkg (or pkg *package*))
  (do-external-symbols (sym pkg)
    (unexport sym pkg)))

;; (defpackage stumpwm (:use :cl))
;; (defun message (&rest args))

;; '(mapcar 'CL-USER::unintern-shadowing-symbols
;;                                (remove-if-not (lambda (pkg)
;;                                                 (ppcre:scan "^YT-COMMENT" (package-name pkg)))
;;                                               (list-all-packages)))
