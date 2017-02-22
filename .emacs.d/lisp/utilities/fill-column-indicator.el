;;; fill-column-indicator.el --- shows the fill column
;;; Commentary:
;;; Code:

(use-package fill-column-indicator
  :ensure t
  :init
  (global-set-key [f3] 'fci-mode))


(provide 'utilities/fill-column-indicator)
;;; fill-column-indicator.el ends here
