;;; view.el --- Emacs view mode setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package view
  :defer
  :config
  (validate-setq
   ;; Open read-only files in view-mode by default.
   view-read-only t))


(provide 'modes/view)
;;; view.el ends here
