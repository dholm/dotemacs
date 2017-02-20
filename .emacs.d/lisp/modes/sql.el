;;; sql.el --- Initializes SQL mode
;;; Commentary:
;;; Code:

(defun user/sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (with-feature 'sql
    (if sql-buffer
        (progn
          (pop-to-buffer sql-buffer)
          (goto-char (point-max)))
      (sql-set-sqli-buffer)
      (when sql-buffer
        (user/sqli-buffer)))))

(use-package sql
  :defer t)


(provide 'modes/sql)
;;; sql.el ends here
