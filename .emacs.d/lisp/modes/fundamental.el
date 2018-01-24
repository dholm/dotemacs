;;; fundamental.el --- Base mode of all other major modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--fundamental-mode-hook ()
  "Fundamental mode hook."
  ;; Automatically break long lines.
  (auto-fill-mode t)

  ;; Enable whitespace mode globally.
  (whitespace-mode t)

  (with-feature 'rainbow-delimiters
    (rainbow-delimiters-mode t))

  ;; Enable dtrt-indent to attempt to identify the indentation rules used.
  (with-eval-after-load 'dtrt-indent
    (dtrt-indent-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :align 'align-current)
  (when (feature-p 'helm)
    (user/bind-key-local :nav :functions/toc 'helm-imenu)))


(defun user--fundamental-mode-config ()
  "Initialize Emacs fundamental mode."
  (validate-setq
   ;; When using fill-paragraph or auto-fill-mode break lines at 80 characters by
   ;; default.
   fill-column 80)

  ;;; (Packages) ;;;
  (use-package rainbow-delimiters)
  (use-package mic-paren
    :config
    (paren-activate))
  (use-package dynamic-spaces
    :config
    (dynamic-spaces-global-mode t))
  (use-package indent-info
    :config
    (global-indent-info-mode t))
  (use-package goldendict
    :if (executable-find "goldendict")
    :bind-wrap
    ((:key :doc :dictionary) . goldendict-dwim)))

(user--fundamental-mode-config)


(provide 'modes/fundamental)
;;; fundamental.el ends here
