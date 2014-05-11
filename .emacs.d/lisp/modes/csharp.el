;;; csharp.el --- C# mode support
;;; Commentary:
;;; Code:

(defun user/csharp-mode-hook ()
  "C# mode hook."
  (unless (derived-mode-p 'prog-mode)
    (user/prog-mode-hook))

  (with-executable 'xbuild
    (setq-local compile-command "xbuild "))

  ;; Bring in CEDET.
  (user/cedet-hook)

  (with-feature 'omnisharp
    (omnisharp-mode t)))


(defun user/csharp-mode-init ()
  "Initialize C# moed."
  (add-hook 'csharp-mode-hook 'user/csharp-mode-hook)

  (after-load 'mode-compile
    (add-to-list 'mode-compile-modes-alist
                 '(csharp-mode . (csharp-invoke-compile-interactively))))

  ;;; (Packages) ;;;
  (require-package '(:name csharp-mode))
  (require-package '(:name omnisharp-mode)))

(user/csharp-mode-init)


(provide 'modes/csharp)
;;; csharp.el ends here
