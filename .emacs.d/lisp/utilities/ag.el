;;; ag.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(defun user/helm-ag-init ()
  "Initialize helm-ag."
  (setq-default
   ;; Insert word at point as search term.
   helm-ag-insert-at-point 'word))


(defun user/ag-init ()
  "Initialize ag."
  (setq-default
   ag-project-root-function
   '(lambda ()
      (with-project project (path-buffer-abs)
        (user/proj-root project))))

  (after-load 'ag
    ;; Search inside compressed files.
    (add-to-list 'ag-arguments "--search-zip"))

  ;;; (Bindings) ;;;
  (if (and (feature-p 'projectile)
           (fboundp 'projectile-ag))
      (global-set-key [remap find-grep] 'projectile-ag)
    (global-set-key [remap find-grep] 'ag)))

(with-executable 'ag
  (when (feature-p 'helm)
    (req-package helm-ag
      :after (user/helm-ag-init)))
  (req-package ag
    :config (user/ag-init)))


(provide 'utilities/ag)
;;; ag.el ends here
