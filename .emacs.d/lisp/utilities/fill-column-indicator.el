;;; fill-column-indicator.el --- shows the fill column
;;; Commentary:
;;; Code:

(defun user--fill-column-indicator-config ()
  "Initialize fill column indicator."
  ;;; (Bindings) ;;;
  (global-set-key [f3] 'fci-mode))

(req-package fill-column-indicator
  :config (user--fill-column-indicator-config))


(provide 'utilities/fill-column-indicator)
;;; fill-column-indicator.el ends here
