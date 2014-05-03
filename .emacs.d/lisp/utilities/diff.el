;;; diff.el --- Configuration for diff
;;; Commentary:
;;; Code:

(defun user/diff-mode-hook ()
  "Diff mode hook."
  (with-feature 'diff-hl
    (turn-on-diff-hl-mode)))


(defun user/diff-init ()
  "Initialize Emacs diff support."
  (setq-default
   ;; Use unified diffs by default.
   diff-switches "-u")

  (add-hook 'diff-mode-hook 'user/diff-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name diff-hl)))

(user/diff-init)


(provide 'utilities/diff)
;;; diff.el ends here
