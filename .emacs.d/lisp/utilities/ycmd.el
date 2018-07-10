;;; ycmd.el --- Support for ycmd -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ycmd
  :disabled
  :if (executable-find "ycmd")
  :hook (ycmd-mode-hook . ycmd-eldoc-setup)
  :init
  (global-ycmd-mode t)
  :config
  (validate-setq
   ;; Location of `ycmd'.
   ycmd-server-command `(,(executable-find "ycmd"))
   ycmd-global-config (path-join *user-home-directory* ".local" "etc"
                                 "ycmd.py")
   ;; Always enable completion.
   ycmd-force-semantic-completion t)

  (use-package company-ycmd
    :after (company)
    :config
    (company-ycmd-setup))

  (use-package flycheck-ycmd
    :config
    (flycheck-ycmd-setup)

    (unless (display-graphic-p)
      (validate-setq
       ;; Workaround for interaction bug between company-ycmd and
       ;; flycheck-ycmd.
       ;;  - https://github.com/abingham/emacs-ycmd/issues/144
       flycheck-indication-mode nil))))


(provide 'utilities/ycmd)
;;; ycmd.el ends here
