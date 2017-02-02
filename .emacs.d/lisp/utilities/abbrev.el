;;; abbrev.el --- Configure Emacs abbreviations
;;; Commentary:
;;; Code:

(defun user--abbrev-config ()
  "Initialize abbrev."
  (validate-setq
   abbrev-file-name (path-join *user-data-directory* "abbrev_defs")))

(user--abbrev-config)


(provide 'utilities/abbrev)
;;; abbrev.el ends here
