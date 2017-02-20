;;; ruby.el --- ruby mode support
;;; Commentary:
;;; Code:

(defun user--ruby-mode-hook ()
  "Ruby mode hook."
  ;; Bring in CEDET.
  (user--cedet-hook)

  (user/gnu-global-enable)

  ;; Enable robe mode
  (robe-mode t)

  ;; Enable inferior ruby mode
  (inf-ruby-minor-mode t)

  ;; Enable eldoc mode
  (eldoc-mode t)

  ;; Separate camel-case into separate words
  (subword-mode t)

  ;; Enable YouCompleteMe.
  (user/ycmd-enable)

  ;;; (Bindings) ;;;
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  (with-feature 'yari
    (user/bind-key-local :doc :reference 'yari)))


(defun user--robe-mode-hook ()
  "Robe mode hook."
  (ac-robe-setup))

(use-package ruby-mode
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'user--ruby-mode-hook)
  :config
  (use-package robe
    :init
    (add-hook 'robe-mode-hook 'user--robe-mode-hook))
  (use-package inf-ruby)
  (use-package yari))


(provide 'modes/ruby)
;;; ruby.el ends here
