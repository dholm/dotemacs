;;; yaml.el --- Initializes YAML mode
;;; Commentary:
;;; Code:

(defun user--yaml-mode-hook ()
  "YAML mode hook.")


(defun user--yaml-mode-config ()
  "Initialize YAML mode."
  ;;; (Hooks) ;;;
  (add-hook 'yaml-mode-hook 'user--yaml-mode-hook))

(req-package yaml-mode
  :config (user--yaml-mode-config))


(provide 'modes/yaml)
;;; yaml.el ends here
