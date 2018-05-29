;;; powerthesaurus.el --- Thesaurus for Emacs. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package powerthesaurus
  :defer
  :bind-wrap
  ((:key :code :thesaurus-lookup) . powerthesaurus-lookup-word))


(provide 'utilities/powerthesaurus)
;;; powerthesaurus.el ends here
