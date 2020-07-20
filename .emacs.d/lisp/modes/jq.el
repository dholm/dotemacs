;;; jq.el --- jq script mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package jq-mode
  :if (executable-find "jq")
  :mode "\\.jq$")


(provide 'modes/jq)
;;; jq.el ends here
