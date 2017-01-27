;;; sql.el --- Initializes SQL mode
;;; Commentary:
;;; Code:

(defun user--sql-mode-hook ()
  "SQL mode hook.")


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


(defun user--sql-config ()
  "Initialize SQL mode."
  (add-hook 'sql-mode-hook 'user--sql-mode-hook))

(user--sql-config)


(provide 'modes/sql)
;;; sql.el ends here
