;;; csharp.el --- C# mode support
;;; Commentary:
;;; Code:

(defun user/csharp-mode-hook ()
  "C# mode hook."
  ;; Bring in CEDET.
  (user/cedet-hook)

  (with-feature 'omnisharp
    (omnisharp-mode t)))


(defun user/csharp-mode-init ()
  "Initialize C# moed."
  (add-hook 'csharp-mode-hook 'user/csharp-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name csharp-mode))
  (require-package '(:name omnisharp-mode)))

(user/csharp-mode-init)


(provide 'modes/csharp)
;;; csharp.el ends here
