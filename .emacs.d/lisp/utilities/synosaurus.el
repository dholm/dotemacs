;;; synosaurus.el --- Thesaurus for Emacs.
;;; Commentary:
;;; Code:

(defun user--synosaurus-config ()
  "Initialize synosaurus."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :thesaurus-lookup 'synosaurus-lookup))

(with-executable 'wn
  (use-package synosaurus
    :ensure t
    :config (user--synosaurus-config)))


(provide 'utilities/synosaurus)
;;; synosaurus.el ends here
