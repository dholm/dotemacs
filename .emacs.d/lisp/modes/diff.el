;;; diff.el --- Configure diff mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--diff-mode-hook ()
  "Diff mode hook."
  ;; Highlight changes in detail.
  (diff-auto-refine-mode t))

(use-package diff-mode
  :defer
  :mode "/patch$"
  :init
  (add-hook 'diff-mode-hook 'user--diff-mode-hook)
  :config
  (validate-setq
   ;; Open patches in read-only mode by default.
   diff-default-read-only t))


(provide 'modes/diff)
;;; diff.el ends here
