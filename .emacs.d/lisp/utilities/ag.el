;;; ag.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(with-executable 'ag
  (when (feature-p 'helm)
    (use-package helm-ag
      :defer
      :config
      (validate-setq
       ;; Insert word at point as search term.
       helm-ag-insert-at-point 'word)))
  (use-package ag
    :defer
    :init
    (if (and (feature-p 'projectile)
             (fboundp 'projectile-ag))
        (global-set-key [remap find-grep] 'projectile-ag)
      (global-set-key [remap find-grep] 'ag))
    :config
    (validate-setq
     ag-project-root-function
     '(lambda ()
        (with-project project (path-buffer-abs)
          (user/proj-root project))))

    ;; Search inside compressed files.
    (add-to-list 'ag-arguments "--search-zip")))


(provide 'utilities/ag)
;;; ag.el ends here
