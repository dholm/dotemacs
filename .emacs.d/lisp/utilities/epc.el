;;; epc.el --- RPC stack for Emacs
;;; Commentary:
;;; Code:

(defun user--epc-config ()
  "Initialize EPC.")

(use-package epc
  :defer t
  :config (user--epc-config))


(provide 'utilities/epc)
;;; epc.el ends here
