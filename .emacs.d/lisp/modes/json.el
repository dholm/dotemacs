;;; json.el --- JSON mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :defer
  :mode "\\.bowerrc$"
  :config
  (use-package json-navigator))


(provide 'modes/json)
;;; json.el ends here
