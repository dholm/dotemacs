;;; ag.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(defun user--helm-ag-config ()
  "Initialize helm-ag."
  (validate-setq
   ;; Insert word at point as search term.
   helm-ag-insert-at-point 'word))


(defun user--ag-config ()
  "Initialize ag."
  (validate-setq
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
    (use-package helm-ag
      :defer t
      :after (user--helm-ag-config)))
  (use-package ag
    :defer t
    :config (user--ag-config)))


(provide 'utilities/ag)
;;; ag.el ends here
