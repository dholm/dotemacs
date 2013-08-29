;;; clearcase --- ClearCase integration
;;; Commentary:
;;; Code:

(defconst *has-cleartool* (and (executable-find "cleartool")
                             ;; Verify that license is valid
                             (eq (call-process-shell-command "cleartool" nil nil nil "quit") 0)))

(when *has-cleartool*
  (require-package '(:name vc-clearcase)))


(provide 'vcs/clearcase)
;;; clearcase.el ends here
