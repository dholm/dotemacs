;;; git.el --- Git integration
;;; Commentary:
;;; Code:

(defun user--git-commit-mode-hook ()
  "Git commit mode hook."
  ;; Run the shared log edit hook.
  (user--vc-log-edit-hook)

  ;;; (Bindings) ;;;
  ;; Override C-x # for Git.
  (local-set-key (kbd "C-x #") 'git-commit-commit)
  ;; Ensure C-c C-c and C-c C-k are bound to git and not org.
  (local-set-key (kbd "C-c C-c") 'git-commit-commit)
  (local-set-key (kbd "C-c C-k") 'git-commit-abort))


(defun user--magit-mode-hook ()
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


(defun user--magit-config ()
  "Initialize Magit."
  (validate-setq
   ;; Automatically show process buffer if git takes too long to execute.
   magit-process-popup-time 30
   ;; Show fine differences for currently selected hunk.
   magit-diff-refine-hunk t)

  (with-feature 'fullframe
    ;; Full frame Magit status.
    (fullframe magit-status magit-mode-quit-window nil))

  ;;; (Hooks) ;;;
  (add-hook 'magit-mode-hook 'user--magit-mode-hook)

  ;;; (Bindings) ;;;
  (define-key magit-status-mode-map (kbd "W") 'user/magit-toggle-whitespace))


(defun user--git-gutter-fringe-config ()
  "Initialize git gutter fringe."
  (validate-setq git-gutter-fr:side 'left-fringe))


(defun user--git-messenger-config ()
  "Initialize git messenger."
  (validate-setq git-messenger:show-detail t))


(defun user--git-config ()
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
  (add-hook 'git-commit-mode-hook 'user--git-commit-mode-hook)

  ;;; (Packages) ;;;
  (use-package magit
    :ensure t
    :config (user--magit-config))

  (use-package magit-gerrit
    :after magit)

  (use-package magit-tramp
    :after magit)

  (use-package git-timemachine
    :ensure t)

  (use-package git-gutter
    :ensure t)

  (use-package git-gutter-fringe
    :if window-system
    :after git-gutter
    :config (user--git-gutter-fringe-config))

  (use-package git-messenger
    :ensure t
    :config (user--git-messenger-config))

  (when (feature-p 'helm)
    (use-package helm-ls-git
      :ensure t)
    (use-package helm-git-grep
      :ensure t
      :commands helm-git-grep)
    (use-package helm-open-github
      :ensure t)
    (use-package helm-hunks
      :ensure t)))

(with-executable 'git
  (user--git-config))


(provide 'vcs/git)
;;; git.el ends here
