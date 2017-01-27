;;; epc.el --- RPC stack for Emacs
;;; Commentary:
;;; Code:

(defun user/epc-init ()
  "Initialize EPC.")

(use-package epc
  :ensure t
  :config (user/epc-init))


(provide 'utilities/epc)
;;; epc.el ends here
