;;; css --- initializes CSS modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--css-mode-hook ()
  "CSS mode hook."
  (validate-setq
   cssm-indent-function #'cssm-c-style-indenter
   cssm-indent-level '4)

  ;; Initialize auto completion
  (tern-ac-setup)
  (ac-css-mode-setup)

  (showcss-mode t)
  (skewer-css-mode t)
  (rainbow-mode t)

  (with-feature 'lsp-css
    (when (eq major-mode 'css-mode)
    ;; Only enable in strictly css-mode, not scss-mode (css-mode-hook
    ;; fires for scss-mode because scss-mode is derived from
    ;; css-mode).
    (lsp-css-enable)))

  ;;; (Bindings) ;;;
  (user/bind-key-local :nav :functions/toc 'helm-css-scss)
  (user/bind-key-local :nav :go-back 'helm-css-scss-back-to-last-point))


(use-package css-mode
  :defer
  :hook (css-mode-hook . user--css-mode-hook)
  :config
  (use-package showcss-mode
    :quelpa (showcss-mode
             :fetcher github
             :repo "smmcg/showcss-mode"))

  (use-package rainbow-mode)

  (when (feature-p 'helm)
    (use-package helm-css-scss))

  (use-package lsp-css
    :if (executable-find "css-languageserver")
    :quelpa (lsp-css
             :fetcher github
             :repo "emacs-lsp/lsp-css"))

  (use-package flycheck-css-colorguard
    :config
    (flycheck-css-colorguard-setup))

  (use-package css-autoprefixer
    :if (executable-find "npx")))


(provide 'modes/css)
;;; css.el ends here
