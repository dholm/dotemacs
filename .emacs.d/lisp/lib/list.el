;;; list.el --- Emacs list utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun add-many-to-list (the-list &rest entries)
  "Add to THE-LIST any specified ENTRIES."
  (dolist (entry entries)
    (add-to-list the-list entry))
  (eval the-list))


(defmacro user/filter-form (form list)
  "Return list with elements for which FORM are non-nil in LIST."
  (declare (debug (form form)))
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (!cons it ,r)))
       (nreverse ,r))))


(defun user/filter-list (condp list)
  "Return list with elements for which CONDP are non-nil in LIST."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) list)))


(defun user/toggle-element (list element)
  "Return LIST with ELEMENT removed if present or added if not present."
  (if (member element list)
      (user/filter-form (not (eq element it)) list)
    (cons element list)))


(defun user/all-asscs (asslist query)
  "A list of all values in ASSLIST corresponding to QUERY (like rassoc)."
  (cond
   ((null asslist) nil)
   (t
    (if (equal (cdr (car asslist)) query)
        (cons (car (car asslist))
              (user/all-asscs (cdr asslist) query))
      (user/all-asscs (cdr asslist) query)))))


(provide 'lib/list)
;;; list.el ends here
