;;; request.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(use-package request
  :defer
  :config
  (validate-setq
   ;; Request cache store.
   request-storage-directory (path-join *user-cache-directory* "request")))


(provide 'utilities/request)
;;; request.el ends here
