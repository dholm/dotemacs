;;; diff.el --- Configuration for diff -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--diff-mode-hook ()
  "Diff mode hook."
  (with-feature 'diff-hl
    (turn-on-diff-hl-mode)))

(use-package diff
  :defer
  :hook (diff-mode-hook . user--diff-mode-hook)
  :config
  (validate-setq
   ;; Use unified diffs by default.
   diff-switches "-u")

  (use-package diff-hl
    :defer)

  (use-package diff-ansi
    :commands (diff-ansi-mode diff-ansi-buffer)))

(use-package dumb-diff
  :defer
  :bind (("C-c 1" . dumb-diff-set-region-as-buffer1)
         ("C-c 2" . dumb-diff-set-region-as-buffer2))
  :init
  (user/bind-key-global :util :dumb-diff 'dumb-diff))


(provide 'utilities/diff)
;;; diff.el ends here
