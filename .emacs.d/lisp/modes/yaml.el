;;; yaml.el --- Initializes YAML mode
;;; Commentary:
;;; Code:

(defun user/yaml-mode-hook ()
  "YAML mode hook.")


(defun user/yaml-mode-init ()
  "Initialize YAML mode."
  ;;; (Hooks) ;;;
  (add-hook 'yaml-mode-hook 'user/yaml-mode-hook))

(require-package '(:name yaml-mode :after (user/yaml-mode-init)))


(provide 'modes/yaml)
;;; yaml.el ends here
