;;; abbrev.el --- Configure Emacs abbreviations
;;; Commentary:
;;; Code:

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :config
  (validate-setq
   abbrev-file-name (path-join *user-data-directory* "abbrev_defs")))


(provide 'utilities/abbrev)
;;; abbrev.el ends here
