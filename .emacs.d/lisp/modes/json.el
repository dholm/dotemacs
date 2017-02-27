;;; json.el --- JSON mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :defer
  :init
  (add-auto-mode 'json-mode "\\.bowerrc$"))


(provide 'modes/json)
;;; json.el ends here
