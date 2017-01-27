;;; llvm.el --- LLVM mode support
;;; Commentary:
;;; Code:

(defun user/llvm-mode-hook ()
  "LLVM mode hook.")


(defun user/tablegen-mode-hook ()
  "TableGen mode hook.")


(defun user/llvm-mode-init ()
  "Initialize LLVM mode."
  (use-package llvm-mode
    :ensure t)
  (require-package '(:name tablegen-mode))

  (add-hook 'llvm-mode-hook 'user/llvm-mode-hook)
  (add-hook 'tablegen-mode-hook 'user/tablegen-mode-hook))

(user/llvm-mode-init)


(provide 'modes/llvm)
;;; llvm.el ends here
