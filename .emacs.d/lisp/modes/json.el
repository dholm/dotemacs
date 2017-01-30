;;; json.el --- JSON mode
;;; Commentary:
;;; Code:

(defun user--json-mode-config ()
  "Initilize JSON mode."
  (add-auto-mode 'json-mode "\\.bowerrc$"))

(use-package json-mode
  :ensure t
  :config (user--json-mode-config))


(provide 'modes/json)
;;; json.el ends here
