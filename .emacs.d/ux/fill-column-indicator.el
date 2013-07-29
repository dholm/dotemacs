;;; fill-column-indicator --- shows the fill column
;;; Commentary:
;;; Code:

(defun dholm/fill-column-indicator-init ()
  "Initialize fill column indicator."
  (after-load 'solarized-theme
    (setq-default
     fci-rule-color (solarized-find-color 'base02 solarized-dark-palette)))

  ;;; (Bindings) ;;;
  (global-set-key [f3] 'fci-mode))


(require-package '(:name fill-column-indicator :after (dholm/fill-column-indicator-init)))


(provide 'ux/fill-column-indicator)
;;; fill-column-indicator.el ends here
