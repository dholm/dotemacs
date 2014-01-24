;;; abbrev.el --- Configure Emacs abbreviations
;;; Commentary:
;;; Code:

(defun user/abbrev-init ()
  "Initialize abbrev."
  (setq-default
   abbrev-file-name (path-join *user-data-directory* "abbrev_defs")))

(user/abbrev-init)


(provide 'utilities/abbrev)
;;; abbrev.el ends here
