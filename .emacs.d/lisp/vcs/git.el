;;; git.el --- Git integration
;;; Commentary:
;;; Code:

(defun user/git-commit-mode-hook ()
  "Git commit mode hook."
  ;; Run the shared log edit hook.
  (user/vc-log-edit-hook)

  ;;; (Bindings) ;;;
  ;; Override C-x # for Git.
  (local-set-key (kbd "C-x #") 'git-commit-commit)
  ;; Ensure C-c C-c and C-c C-k are bound to git and not org.
  (local-set-key (kbd "C-c C-c") 'git-commit-commit)
  (local-set-key (kbd "C-c C-k") 'git-commit-abort))


(defun user/magit-mode-hook ()
  "Magit mode hook.")


(defun user/magit-toggle-whitespace ()
  "Toggle showing all differences in whitespace when using Magit."
  (interactive)
  (if (member "-w" magit-diff-options)
      (progn
        (message "Showing differences in whitespace.")
        (setq magit-diff-options (remove "-w" magit-diff-options)))
    (progn
      (message "Hiding differences in whitespace.")
      (add-to-list 'magit-diff-options "-w")))
  (magit-refresh))


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

  (add-hook 'magit-mode-hook 'user/magit-mode-hook)

  ;;; (Bindings) ;;;
  (after-load 'magit
    (define-key magit-status-mode-map (kbd "W") 'user/magit-toggle-whitespace)))


(defun user/git-gutter-fringe-init ()
  "Initialize git gutter fringe."
  (setq-default git-gutter-fr:side 'left-fringe))


(defun user/git-messenger-init ()
  "Initialize git messenger."
  (setq-default git-messenger:show-detail t))


(defun user/git-init ()
  "Initialize Git support."
  (add-hook 'git-commit-mode-hook 'user/git-commit-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name magit :after (user/magit-init)))
  (require-package '(:name magit-gerrit))
  (require-package '(:name magit-tramp))
  (require-package '(:name git-timemachine))

  (require-package '(:name git-gutter))
  (when (display-graphic-p)
    (require-package '(:name git-gutter-fringe :after (user/git-gutter-fringe-init))))
  (require-package '(:name git-messenger :after (user/git-messenger-init)))
  (with-executable 'git-review
    (require-package '(:name gerrit-download)))
  (when (feature-p 'helm)
    (require-package '(:name helm-ls-git))
    (require-package '(:name helm-open-github))))

(with-executable 'git
  (user/git-init))


(provide 'vcs/git)
;;; git.el ends here
