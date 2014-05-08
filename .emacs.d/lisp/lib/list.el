;;; list.el --- Emacs list utilities
;;; Commentary:
;;; Code:

(defun add-many-to-list (the-list &rest entries)
  "Add to THE-LIST any specified ENTRIES."
  (dolist (entry entries)
    (add-to-list the-list entry))
  (eval the-list))


(defun user/filter (condp lst)
  "Return list with elements for which CONDP are non-nil in LST."
  (delq nil (mapcar (lambda (elt) (and (funcall condp elt) elt)) lst)))


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
