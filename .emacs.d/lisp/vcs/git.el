;;; git.el --- Git integration
;;; Commentary:
;;; Code:

(defun user/git-gutter-fringe-init ()
  "Initialize git gutter fringe."
  (setq-default git-gutter-fr:side 'left-fringe))


(defun user/magit-mode-hook ()
  "Magit mode hook."
  ;; Ignore change in whitespace by default.
  (add-to-list 'magit-diff-options "--ignore-space-at-eol"))


(defun user/magit-init ()
  "Initialize Magit."
  (setq-default
   ;; Do not save buffers.
   magit-save-some-buffers nil
   ;; Automatically show process buffer if git takes too long to execute.
   magit-process-popup-time 30
   ;; Show fine differences for currently selected hunk.
   magit-diff-refine-hunk t)

  ;; Full frame Magit status.
  (with-feature 'fullframe
    (fullframe magit-status magit-mode-quit-window nil))

  (add-hook 'magit-mode-hook 'user/magit-mode-hook))


(defun user/git-messenger-init ()
  "Initialize git messenger."
  (setq-default git-messenger:show-detail t))


(defun user/git-init ()
  "Initialize Git support."
  (add-hook 'git-commit-mode-hook 'user/vc-log-edit-hook)

  ;;; (Packages) ;;;
  (require-package '(:name magit :after (user/magit-init)))
  (require-package '(:name git-modes))
  (require-package '(:name git-gutter))
  (when (display-graphic-p)
    (require-package '(:name git-gutter-fringe :after (user/git-gutter-fringe-init))))
  (require-package '(:name git-messenger :after (user/git-messenger-init)))
  (with-executable 'git-review
    (require-package '(:name gerrit-download)))
  (require-package '(:name magit-gerrit)))

(with-executable 'git
  (user/git-init))


(provide 'vcs/git)
;;; git.el ends here
