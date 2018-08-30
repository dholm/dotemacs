;;; bap.el --- Initializes BAP mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bap-mode
  :if (executable-find "bap")
  :bind-wrap
  (:map bap-mode-map
        ((:key :nav :follow-symbol) . bap-goto-function-definition)))


(provide 'modes/bap)
;;; bap.el ends here
