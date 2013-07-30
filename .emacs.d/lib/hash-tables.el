;;; hash-tables --- utilities for working with LISP hash tables
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))


(defun hash-table-to-list (hash-table)
  "Return a list that represent the HASH-TABLE."
  (let (ll)
    (maphash (lambda (kk vv) (setq ll (cons (list kk vv) ll))) hash-table)
    ll))

(defun hash-table-keys (hash-table)
  "Return a list of the keys in the HASH-TABLE."
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(cl-defmacro do-hash-table-sorted-by-value ((x hash-table) &rest body)
  "Iterates over keys in HASH-TABLE sorted by value with keys accessible as X
to BODY."
  (declare (indent defun))
  `(dolist (,x (sort (hash-table-keys ,hash-table)
                     (lambda (k1 k2)
                       (< (gethash k1 ,hash-table)
                          (gethash k2 ,hash-table)))))
     ,@body))


(provide 'lib/hash-tables)
;;; hash-tables.el ends here
