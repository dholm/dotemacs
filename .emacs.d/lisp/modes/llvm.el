;;; llvm.el --- LLVM mode support
;;; Commentary:
;;; Code:

(defun user--llvm-mode-hook ()
  "LLVM mode hook.")


(defun user--tablegen-mode-hook ()
  "TableGen mode hook.")


(use-package llvm-mode
  :defer t
  :config
  (use-package tablegen-mode
    :quelpa (tablegen-mode
             :fetcher url
             :url "https://raw.githubusercontent.com/llvm-mirror/llvm/master/utils/emacs/tablegen-mode.el")
    :init
    (add-auto-mode 'tablegen-mode "\\.td\\'")
    :config
    (add-hook 'tablegen-mode-hook 'user--tablegen-mode-hook))
  (add-hook 'llvm-mode-hook 'user--llvm-mode-hook))


(provide 'modes/llvm)
;;; llvm.el ends here
