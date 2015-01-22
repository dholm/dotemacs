;;; diff.el --- Configure diff mode
;;; Commentary:
;;; Code:

(defun user/diff-mode-hook ()
  "Diff mode hook."
  ;; Highlight changes in detail.
  (diff-auto-refine-mode t))


(defun user/diff-mode-init ()
  "Initialize Diff mode."
  (setq-default
   ;; Open patches in read-only mode by default.
   diff-default-read-only t)

  (add-hook 'diff-mode-hook 'user/diff-mode-hook))

(user/diff-mode-init)


(provide 'modes/csv)
;;; diff.el ends here
