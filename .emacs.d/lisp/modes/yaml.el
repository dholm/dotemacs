;;; yaml.el --- Initializes YAML mode
;;; Commentary:
;;; Code:

(defun user/yaml-mode-hook ()
  "YAML mode hook.")


(defun user/yaml-mode-init ()
  "Initialize YAML mode."
  ;;; (Hooks) ;;;
  (add-hook 'yaml-mode-hook 'user/yaml-mode-hook))

(use-package yaml-mode
  :ensure t
  :config (user/yaml-mode-init))


(provide 'modes/yaml)
;;; yaml.el ends here
