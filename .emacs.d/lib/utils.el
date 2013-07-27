;;; utils --- miscellaneous support functions
;;; Commentary:
;;; Code:

(defmacro try-eval (fn &optional finally)
  "Safely evaluate expression FN and run FINALLY after."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@finally))


(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


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
  `(dolist (,x (sort (hash-table-keys ,hash-table)
                     (lambda (k1 k2)
                       (< (gethash k1 ,hash-table)
                          (gethash k2 ,hash-table)))))
     ,@body))


(provide 'lib/utils)
;;; utils.el ends here
