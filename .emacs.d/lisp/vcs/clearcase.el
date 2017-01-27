;;; clearcase.el --- ClearCase integration
;;; Commentary:
;;; Code:

(with-executable 'cleartool
  ;; Verify that license is valid
  (when (eq (call-process-shell-command "cleartool" nil nil nil "quit") 0)
    (req-package vc-clearcase
      :loader :el-get)))


(provide 'vcs/clearcase)
;;; clearcase.el ends here
