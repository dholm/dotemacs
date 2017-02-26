;;; synosaurus.el --- Thesaurus for Emacs.
;;; Commentary:
;;; Code:

(with-executable 'wn
  (use-package synosaurus
    :defer
    :init
    (user/bind-key-global :code :thesaurus-lookup 'synosaurus-lookup)))


(provide 'utilities/synosaurus)
;;; synosaurus.el ends here
