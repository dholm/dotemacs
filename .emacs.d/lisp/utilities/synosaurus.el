;;; synosaurus.el --- Thesaurus for Emacs.
;;; Commentary:
;;; Code:

(defun user/synosaurus-init ()
  "Initialize synosaurus."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :thesaurus-lookup 'synosaurus-lookup))

(with-executable 'wn
  (require-package '(:name synosaurus :after (user/synosaurus-init))))


(provide 'utilities/synosaurus)
;;; synosaurus.el ends here
