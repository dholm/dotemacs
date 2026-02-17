;;; yaml.el --- Initializes YAML mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--yaml-mode-hook ()
  "YAML mode hook."
  (with-feature 'yaml-pro
    (cond
     ((eq major-mode 'yaml-mode) (yaml-pro-mode t))
     ((eq major-mode 'yaml-ts-mode) (yaml-pro-ts-mode t))))

  (with-feature 'go-template-helper-mode
    (go-template-helper-mode t)))


(use-package yaml-mode
  :defer
  :hook ((yaml-mode-hook . user--yaml-mode-hook)
         (yaml-ts-mode-hook . user--yaml-mode-hook))
  :config
  (use-package flycheck-yamllint
    :if (executable-find "yamllint")
    :config
    (flycheck-yamllint-setup))

  (use-package yaml-pro)

  (use-package yaml-imenu
    :config
    (yaml-imenu-enable))

  (use-package go-template-helper-mode))


(provide 'modes/yaml)
;;; yaml.el ends here
