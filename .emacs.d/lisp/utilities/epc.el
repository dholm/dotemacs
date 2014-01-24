;;; epc --- RPC stack for Emacs
;;; Commentary:
;;; Code:

(defun user/epc-init ()
  "Initialize EPC.")

(require-package '(:name epc :after (user/epc-init)))


(provide 'utilities/epc)
;;; epc.el ends here
