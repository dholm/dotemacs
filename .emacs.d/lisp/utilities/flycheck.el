;;; flycheck.el --- flycheck configuration
;;; Commentary:
;;; Code:

(defun user/flycheck-mode-hook ()
  "Flycheck mode hook."
  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "E") 'flycheck-list-errors)
  (define-key user/navigation-map (kbd "e") 'flycheck-next-error))


(defun user/flycheck-color-mode-line-init ()
  "Initialize flycheck color mode."
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


(defun user/flycheck-init ()
  "Initialize flycheck."
  (require 'flycheck)

  (setq-default
   ;; Wait five seconds before starting checker
   flycheck-idle-change-delay 5.0)
  (global-flycheck-mode t)

  (after-load 'popwin
    ;; Use popwin for Flycheck error list.
    (push '(flycheck-error-list-mode :stick t) popwin:special-display-config))

  (add-hook 'flycheck-mode-hook 'user/flycheck-mode-hook))

(require-package '(:name flycheck :after (user/flycheck-init)))


(provide 'utilities/flycheck)
;;; flycheck.el ends here
