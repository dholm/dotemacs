;;; synosaurus.el --- Thesaurus for Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package synosaurus
  :if (executable-find "wn")
  :defer
  :init
  (user/bind-key-global :code :thesaurus-lookup 'synosaurus-lookup))


(provide 'utilities/synosaurus)
;;; synosaurus.el ends here
