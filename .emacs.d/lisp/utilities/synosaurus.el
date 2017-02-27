;;; synosaurus.el --- Thesaurus for Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-executable 'wn
  (use-package synosaurus
    :defer
    :init
    (user/bind-key-global :code :thesaurus-lookup 'synosaurus-lookup)))


(provide 'utilities/synosaurus)
;;; synosaurus.el ends here
