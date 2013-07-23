;;; csv --- initializes CSV modes
;;; Commentary:
;;; Code:

(require-package '(:name csv-mode))
(require-package '(:name csv-nav))

(setq csv-separators '("," ";" "|" " "))


(provide 'modes/csv)
;;; csv.el ends here
