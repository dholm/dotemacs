;;; synosaurus.el --- Thesaurus for Emacs.
;;; Commentary:
;;; Code:

(defun user/synosaurus-init ()
  "Initialize synosaurus."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :thesaurus-lookup 'synosaurus-lookup))

(with-executable 'wn
  (req-package synosaurus
    :config (user/synosaurus-init)))


(provide 'utilities/synosaurus)
;;; synosaurus.el ends here
