;;; csv --- initializes CSV modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--csv-mode-hook ()
  "CSV mode hook."
  (setq
   ;; Do not wrap long lines.
   truncate-lines t)

  (turn-off-auto-fill)
  (user/whitespace-disable-style '(lines)))

(use-package csv-mode
  :defer
  :mode "\\.[Cc][Ss][Vv]$"
  :init
  (add-hook 'csv-mode-hook 'user--csv-mode-hook)
  :config
  (validate-setq
   ;; Default separators for CSV files.
   csv-separators '("," ";" "|" " " "\t")
   ;; Number of lines to consider part of header.
   csv-header-lines 1))


(provide 'modes/csv)
;;; csv.el ends here
