;;; clearcase.el --- ClearCase integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-executable 'cleartool
  ;; Verify that license is valid
  (when (eq (call-process-shell-command "cleartool" nil nil nil "quit") 0)
    (use-package vc-clearcase
      :defer
      :quelpa (vc-clearcase
               :fetcher github
               :repo "alex-hhh/vc-clearcase"))))


(provide 'vcs/clearcase)
;;; clearcase.el ends here
