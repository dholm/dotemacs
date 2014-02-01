;;; ag.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(defun user/ag-init ()
  "Initialize ag."
  (setq-default
   ag-project-root-function `(user/current-path-apply 'user/project-root))

  ;;; (Bindings) ;;;
  (global-set-key [remap find-grep] 'ag))

(when *has-ag*
  (require-package '(:name ag :after (user/ag-init))))


(provide 'utilities/ag)
;;; ag.el ends here
