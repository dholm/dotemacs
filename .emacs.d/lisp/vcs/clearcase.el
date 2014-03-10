;;; clearcase.el --- ClearCase integration
;;; Commentary:
;;; Code:

(with-executable 'cleartool
  ;; Verify that license is valid
  (when (eq (call-process-shell-command "cleartool" nil nil nil "quit") 0)
    (require-package '(:name vc-clearcase))))


(provide 'vcs/clearcase)
;;; clearcase.el ends here
