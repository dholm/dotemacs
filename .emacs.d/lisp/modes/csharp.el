;;; csharp.el --- C# mode support
;;; Commentary:
;;; Code:

(defun user/csharp-mode-hook ()
  "C# mode hook."
  (unless (derived-mode-p 'prog-mode)
    (user/prog-mode-hook))

  (user/gnu-global-enable)

  (with-executable 'xbuild
    (setq-local compile-command "xbuild "))

  ;; Still indent inside namespace.
  (c-set-offset 'innamespace '+)

  ;; Bring in CEDET.
  (user/cedet-hook)

  (with-feature 'omnisharp
    (omnisharp-mode t)
    (eldoc-mode t)
    (add-ac-sources 'ac-source-omnisharp)))


(defun user/sln-mode-init ()
  "Initialize sln mode."
  (add-auto-mode 'sln-mode "\\.sln$"))


(defun user/csharp-mode-init ()
  "Initialize C# mode."
  (add-hook 'csharp-mode-hook 'user/csharp-mode-hook)

  (after-load 'csharp-mode
    (add-to-list 'c-default-style '(csharp-mode . "Google")))

  (after-load 'mode-compile
    (add-to-list 'mode-compile-modes-alist
                 '(csharp-mode . (csharp-invoke-compile-interactively))))

  ;;; (Packages) ;;;
  (require-package '(:name csharp-mode))
  (require-package '(:name omnisharp-mode))
  (require-package '(:name sln-mode :after (user/sln-mode-init))))

(user/csharp-mode-init)


(provide 'modes/csharp)
;;; csharp.el ends here
