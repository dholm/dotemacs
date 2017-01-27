;;; epc.el --- RPC stack for Emacs
;;; Commentary:
;;; Code:

(defun user/epc-init ()
  "Initialize EPC.")

(req-package epc
  :config (user/epc-init))


(provide 'utilities/epc)
;;; epc.el ends here
