;;; json.el --- JSON mode
;;; Commentary:
;;; Code:

(use-package json-mode
  :defer t
  :init
  (add-auto-mode 'json-mode "\\.bowerrc$"))


(provide 'modes/json)
;;; json.el ends here
