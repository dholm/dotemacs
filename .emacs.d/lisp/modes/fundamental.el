;;; fundamental.el --- Base mode of all other major modes
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
  (after-load 'dtrt-indent
    (dtrt-indent-mode t))

  (with-feature 'annotate
    ;; Enable annotations.
    (annotate-mode t))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :align 'align-current)
  (when (feature-p 'helm)
    (user/bind-key-local :nav :functions/toc 'helm-imenu)))


(defun user--mic-paren-config ()
  "Initialize mic-paren."
  (paren-activate))


(defun user--fundamental-mode-config ()
  "Initialize Emacs fundamental mode."
  (validate-setq
   ;; When using fill-paragraph or auto-fill-mode break lines at 80 characters by
   ;; default.
   fill-column 80)

  (after-load 'diminish
    ;; Diminish common modes.
    (diminish 'abbrev-mode)
    (diminish 'auto-fill-function))

  ;;; (Packages) ;;;
  (use-package rainbow-delimiters
    :ensure t)
  (use-package mic-paren
    :ensure t
    :config (user--mic-paren-config)))

(user--fundamental-mode-config)


(provide 'modes/fundamental)
;;; fundamental.el ends here
