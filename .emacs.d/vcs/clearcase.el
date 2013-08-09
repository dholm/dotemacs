;;; clearcase --- ClearCase integration
;;; Commentary:
;;; Code:

(defconst *has-cleartool* (executable-find "cleartool"))

(when *has-cleartool*
  (require-package '(:name vc-clearcase)))


(provide 'vcs/clearcase)
;;; clearcase.el ends here
