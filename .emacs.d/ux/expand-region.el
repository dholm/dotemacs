;;; expand-region --- expand regions by logical increments
;;; Commentary:
;;; Code:

(defun dholm/expand-region-init ()
  "Initialize expand region."
  (global-set-key (kbd "C-=") 'er/expand-region))

(require-package '(:name expand-region :after (dholm/expand-region-init)))


(provide 'ux/expand-region)
;;; expand-region.el ends here
