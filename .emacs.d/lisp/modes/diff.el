;;; diff.el --- Configure diff mode
;;; Commentary:
;;; Code:

(defun user--diff-mode-hook ()
  "Diff mode hook."
  ;; Highlight changes in detail.
  (diff-auto-refine-mode t))

(use-package diff-mode
  :defer
  :init
  (add-auto-mode 'diff-mode "/patch$")
  (add-hook 'diff-mode-hook 'user--diff-mode-hook)
  :config
  (validate-setq
   ;; Open patches in read-only mode by default.
   diff-default-read-only t))


(provide 'modes/diff)
;;; diff.el ends here
