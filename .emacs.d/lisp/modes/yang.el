;;; yang --- Initializes YANG mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--yang-mode-hook ()
  "YANG mode hook."
  (when (feature-p 'flycheck-yang)
    (flycheck-mode t)))

(use-package yang-mode
  :defer
  :mode "\.yang$"
  :hook (yang-mode-hook . user--yang-mode-hook)
  :config
  (use-package flycheck-yang
    :if (executable-find "pyang")))


(provide 'modes/yang)
;;; yang.el ends here
