;;; yaml.el --- Initializes YAML mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :defer
  :config
  (use-package flycheck-yamllint
    :if (executable-find "yamllint")
    :config
    (flycheck-yamllint-setup)))


(provide 'modes/yaml)
;;; yaml.el ends here
