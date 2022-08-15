;;; yaml.el --- Initializes YAML mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :defer
  :config
  (use-package flycheck-yamllint
    :if (executable-find "yamllint")
    :config
    (flycheck-yamllint-setup))

  (use-package yaml-pro
    :hook (yaml-mode . yaml-pro-mode))

  (use-package yaml-imenu
    :config
    (yaml-imenu-enable)))


(provide 'modes/yaml)
;;; yaml.el ends here
