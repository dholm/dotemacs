;;; fill-column-indicator.el --- shows the fill column
;;; Commentary:
;;; Code:

(defun user/fill-column-indicator-init ()
  "Initialize fill column indicator."
  ;;; (Bindings) ;;;
  (global-set-key [f3] 'fci-mode))

(require-package '(:name fill-column-indicator :after (user/fill-column-indicator-init)))


(provide 'utilities/fill-column-indicator)
;;; fill-column-indicator.el ends here