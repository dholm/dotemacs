;;; diff.el --- Configuration for diff
;;; Commentary:
;;; Code:

(defun user--diff-mode-hook ()
  "Diff mode hook."
  (with-feature 'diff-hl
    (turn-on-diff-hl-mode)))

(use-package diff
  :defer
  :init
  (add-hook 'diff-mode-hook 'user--diff-mode-hook)
  :config
  (validate-setq
   ;; Use unified diffs by default.
   diff-switches "-u")

  (use-package diff-hl
    :defer))


(provide 'utilities/diff)
;;; diff.el ends here
