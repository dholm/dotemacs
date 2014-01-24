;;; ag.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(defun user/ag-init ()
  "Initialize ag."
  (setq-default
   ag-project-root-function `(user/current-path-apply 'user/project-root))

  ;;; (Bindings) ;;;
  (define-key user/navigation-map (kbd "f") 'ag))


(when *has-ag*
  (require-package '(:name ag :after (user/ag-init))))


(provide 'utilities/minimap)
;;; ag.el ends here
