;;; ruby.el --- ruby mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--ruby-mode-hook ()
  "Ruby mode hook."
  ;; Bring in CEDET.
  (user--cedet-hook)

  (user/gnu-global-enable)

  ;; Enable inferior ruby mode
  (inf-ruby-minor-mode t)

  ;; Enable eldoc mode
  (eldoc-mode t)

  ;; Separate camel-case into separate words
  (subword-mode t))


(defun user--robe-mode-hook ()
  "Robe mode hook."
  (when (user/auto-complete-p)
    (ac-robe-setup)))

(use-package ruby-mode
  :defer
  :hook (ruby-mode-hook . user--ruby-mode-hook)
  :bind
  (:map ruby-mode-map
        ("TAB" . indent-for-tab-command))
  :config
  (use-package robe
    :hook
    ((ruby-mode-hook . robe-mode)
     (robe-mode-hook . user--robe-mode-hook))
    :config
    (eval-after-load 'company
      (push 'company-robe company-backends)))
  (use-package inf-ruby)
  (use-package yari
    :bind-wrap
    (:map ruby-mode-map
          ((:key :doc :reference) . yari)))
  (use-package rubocopfmt
    :if (executable-find "rubocop")
    :hook (ruby-mode-hook . rubocopfmt-mode))
  (use-package lsp-ruby
    :if (executable-find "solargraph")
    :hook (ruby-mode-hook . lsp-ruby-enable)))


(provide 'modes/ruby)
;;; ruby.el ends here
