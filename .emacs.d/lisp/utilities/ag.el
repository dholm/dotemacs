;;; ag.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(defun user/ag-init ()
  "Initialize ag."
  (setq-default
   ag-project-root-function '(lambda ()
                               (user/project-root (path-abs-buffer))))

  ;;; (Bindings) ;;;
  (if (and (feature-p 'projectile)
           (fboundp 'projectile-ag))
      (global-set-key [remap find-grep] 'projectile-ag)
    (global-set-key [remap find-grep] 'ag)))

(with-executable 'ag
  (require-package '(:name ag :after (user/ag-init))))


(provide 'utilities/ag)
;;; ag.el ends here
