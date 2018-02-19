;;; git.el --- Git integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--git-commit-mode-hook ()
  "Git commit mode hook."
  ;; Run the shared log edit hook.
  (user--vc-log-edit-hook)

  ;;; (Bindings) ;;;
  ;; Ensure C-c C-c and C-c C-k are bound to git and not org.
  (local-set-key (kbd "C-c C-c") 'git-commit-commit)
  (local-set-key (kbd "C-c C-k") 'git-commit-abort))


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


(defun user--git-config ()
  "Initialize Git support."
  ;; Automatic conf-mode expressions.
  (add-auto-mode 'conf-mode "\\.git\\(config\\|attributes\\|ignore\\)\\(\\.local\\)?$")

  (use-package git-commit
    :ensure nil
    :hook (git-commit-mode-hook . user--git-commit-mode-hook)
    :bind-wrap
    (:map git-commit-mode-map
          ((:key :basic :server-edit) . git-commit-commit)))

  ;;; (Packages) ;;;
  (use-package magit
    :defer
    :config
    (validate-setq
     ;; Automatically show process buffer if git takes too long to execute.
     magit-process-popup-time 30
     ;; Show fine differences for currently selected hunk.
     magit-diff-refine-hunk t)

    (with-eval-after-load 'popwin
      ;; Use popwin for certain Magit buffers.
      (add-many-to-list
       'popwin:special-display-config
       '("*magit-edit-log*" :noselect t :height 0.2 :width 80)
       '("*magit-process*" :noselect t :height 0.2 :width 80)))

    (with-feature 'fullframe
      ;; Full frame Magit status.
      (fullframe magit-status magit-mode-quit-window nil))

    ;;; (Bindings) ;;;
    (define-key magit-status-mode-map (kbd "W")
      'user/magit-toggle-whitespace)

    (use-package magit-gerrit)
    (use-package magit-tramp)
    (use-package magithub
      :config
      (validate-setq
       magithub-dir (path-join *user-data-directory* "magithub"))

      (unless (fboundp #'ghub-request)
        ;; Compatibility with new version of ghub.
        (defalias 'ghub-request #'ghub--request))

      (magithub-feature-autoinject t))
    (use-package magit-imerge
      :if (executable-find "git-imerge")))

  (use-package git-timemachine
    :defer)

  (use-package git-gutter
    :defer
    :config
    (use-package git-gutter-fringe
      :if window-system
      :defer
      :config
      (validate-setq git-gutter-fr:side 'left-fringe)))

  (use-package git-messenger
    :defer
    :config
    (validate-setq git-messenger:show-detail t))

  (when (feature-p 'helm)
    (use-package helm-ls-git
      :defer)
    (use-package helm-git-grep
      :defer
      :pin "MELPA"
      :commands helm-git-grep)
    (use-package helm-open-github
      :defer)
    (use-package helm-hunks
      :defer)))

(with-executable 'git
  (user--git-config))


(provide 'vcs/git)
;;; git.el ends here
