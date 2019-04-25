;;; json.el --- JSON mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :defer
  :mode "\\.bowerrc$"
  :config
  (use-package json-navigator)
  (use-package json-reformatter-jq
    :if (executable-find "jq")))

(use-package jsonnet-mode)


(provide 'modes/json)
;;; json.el ends here
