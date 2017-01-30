;;; llvm.el --- LLVM mode support
;;; Commentary:
;;; Code:

(defun user--llvm-mode-hook ()
  "LLVM mode hook.")


(defun user--tablegen-mode-hook ()
  "TableGen mode hook.")


(defun user--llvm-mode-config ()
  "Initialize LLVM mode."
  (use-package llvm-mode
    :defer t)
  (require-package '(:name tablegen-mode))

  (add-hook 'llvm-mode-hook 'user--llvm-mode-hook)
  (add-hook 'tablegen-mode-hook 'user--tablegen-mode-hook))

(user--llvm-mode-config)


(provide 'modes/llvm)
;;; llvm.el ends here
