;;; json.el --- JSON mode
;;; Commentary:
;;; Code:

(defun user/json-mode-init ()
  "Initilize JSON mode."
  (add-auto-mode 'json-mode "\\.bowerrc$"))

(require-package '(:name json-mode :after (user/json-mode-init)))


(provide 'modes/json)
;;; json.el ends here
