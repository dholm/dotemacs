;;; diff.el --- Configuration for diff
;;; Commentary:
;;; Code:

(defun user--diff-mode-hook ()
  "Diff mode hook."
  (with-feature 'diff-hl
    (turn-on-diff-hl-mode)))


(defun user--diff-config ()
  "Initialize Emacs diff support."
  (setq-default
   ;; Use unified diffs by default.
   diff-switches "-u")

  (add-hook 'diff-mode-hook 'user--diff-mode-hook)

  ;;; (Packages) ;;;
  (req-package diff-hl))

(user--diff-config)


(provide 'utilities/diff)
;;; diff.el ends here
