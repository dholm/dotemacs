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
   magit-diff-refine-hunk t
   ;; Suppress Magit upgrade warning.
   magit-last-seen-setup-instructions "1.4.0")

  (after-load 'magit
    (with-feature 'fullframe
      ;; Full frame Magit status.
      (fullframe magit-status magit-mode-quit-window nil)))

  (add-hook 'magit-mode-hook 'user/magit-mode-hook)

  ;;; (Bindings) ;;;
  (after-load 'magit
    (define-key magit-status-mode-map (kbd "W") 'user/magit-toggle-whitespace)))


(defun user/magit-gerrit-init ()
  "Initialize magit-gerrit."
  (setq-default
   ;; Magit binding for Gerrit commands.
   magit-gerrit-popup-prefix "G"))


(defun user/git-gutter-fringe-init ()
  "Initialize git gutter fringe."
  (setq-default git-gutter-fr:side 'left-fringe))


(defun user/git-messenger-init ()
  "Initialize git messenger."
  (setq-default git-messenger:show-detail t))


(defun user/git-init ()
  "Initialize Git support."
  (after-load 'popwin
    ;; Use popwin for certain Magit buffers.
    (add-many-to-list
     'popwin:special-display-config
     '("*magit-edit-log*" :noselect t :height 0.2 :width 80)
     '("*magit-process*" :noselect t :height 0.2 :width 80)))

  ;; Automatic conf-mode expressions.
  (add-auto-mode 'conf-mode "\\.git\\(config\\|attributes\\|ignore\\)\\(\\.local\\)?$")

  ;;; (Hooks) ;;;
  (add-hook 'git-commit-mode-hook 'user/git-commit-mode-hook)

  ;;; (Packages) ;;;
  (req-package magit
    :config (user/magit-init))
  (req-package magit-gerrit
    :config (user/magit-gerrit-init))
  (req-package magit-tramp)
  (req-package git-timemachine)

  (req-package git-gutter)
  (when (display-graphic-p)
    (req-package git-gutter-fringe
      :config (user/git-gutter-fringe-init)))
  (req-package git-messenger
    :config (user/git-messenger-init))
  (when (feature-p 'helm)
    (req-package helm-ls-git)
    (req-package helm-git-grep)
    (req-package helm-open-github)
    (req-package helm-hunks)))

(with-executable 'git
  (user/git-init))


(provide 'vcs/git)
;;; git.el ends here
