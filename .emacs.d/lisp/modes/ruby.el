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


(defun user--robe-config ()
  "Initialize robe."
  (add-hook 'robe-mode-hook 'user--robe-mode-hook))


(defun user--ruby-mode-config ()
  "Initialize Ruby mode."
  (req-package ruby-mode)
  (req-package robe
    :config (user--robe-config))
  (req-package inf-ruby)
  (req-package yari)

  (add-hook 'ruby-mode-hook 'user--ruby-mode-hook))

(with-executable 'ruby
  (user--ruby-mode-config))


(provide 'modes/ruby)
;;; ruby.el ends here
