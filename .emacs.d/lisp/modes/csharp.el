;;; csharp.el --- C# mode support
;;; Commentary:
;;; Code:

(defconst *user-omnisharp-path*
  (path-join *user-home-directory* ".local" "opt" "omnisharp"))


(defun user--csharp-mode-hook ()
  "C# mode hook."
  (unless (derived-mode-p 'prog-mode)
    (user--prog-mode-hook))

  (with-executable 'xbuild
    (setq-local compile-command "xbuild "))

  ;; Still indent inside namespace.
  (c-set-offset 'innamespace '+)

  ;; Bring in CEDET.
  (user--cedet-hook)
  (with-feature 'omnisharp
    (eldoc-mode t)
    (add-ac-sources 'ac-source-omnisharp)
    (add-company-sources 'company-omnisharp)
    (omnisharp-mode t)))


(defun user--sln-mode-config ()
  "Initialize sln mode."
  (add-auto-mode 'sln-mode "\\.sln$"))


(defun user--omnisharp-config ()
  "Initialize omnisharp."
  (setq-default
   omnisharp-server-executable-path (path-join *user-omnisharp-path* "omnisharp")))


(defun user--csharp-mode-config ()
  "Initialize C# mode."
  (add-hook 'csharp-mode-hook 'user--csharp-mode-hook)

  (after-load 'csharp-mode
    (add-to-list 'c-default-style '(csharp-mode . "Google")))

  (after-load 'mode-compile
    (add-to-list 'mode-compile-modes-alist
                 '(csharp-mode . (csharp-invoke-compile-interactively))))

  ;;; (Packages) ;;;
  (req-package csharp-mode)
  (when (file-exists-p *user-omnisharp-path*)
    (req-package omnisharp
      :config (user--omnisharp-config)))
  (req-package sln-mode
    :loader :el-get
    :config (user--sln-mode-config)))

(user--csharp-mode-config)


(provide 'modes/csharp)
;;; csharp.el ends here
