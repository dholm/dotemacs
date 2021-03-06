;;; json.el --- JSON mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :defer
  :mode "\\.bowerrc$"
  :config
  (use-package json-navigator)
  (use-package jq-format
    :if (executable-find "jq")))

(use-package jsonnet-mode
  :pin "MELPA")


(provide 'modes/json)
;;; json.el ends here
