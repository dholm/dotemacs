;;; csv --- initializes CSV modes
;;; Commentary:
;;; Code:

(defun user/csv-mode-init ()
  "Initialize CSV mode."
  (setq-default csv-separators '("," ";" "|" " "))
  (add-auto-mode 'csv-mode "\\.csv$"))

(require-package '(:name csv-mode :after (user/csv-mode-init)))
(require-package '(:name csv-nav))


(provide 'modes/csv)
;;; csv.el ends here
