;;; json.el --- JSON mode
;;; Commentary:
;;; Code:

(defun user/json-mode-init ()
  "Initilize JSON mode."
  (add-auto-mode 'json-mode "\\.bowerrc$"))

(use-package json-mode
  :ensure t
  :config (user/json-mode-init))


(provide 'modes/json)
;;; json.el ends here
