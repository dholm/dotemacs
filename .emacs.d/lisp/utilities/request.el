;;; request.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(defun user--request-config ()
  "Initialize request."
  (setq-default
   ;; Request cache store.
   request-storage-directory (path-join *user-cache-directory* "request")))

(use-package request
  :ensure t
  :config (user--request-config))


(provide 'utilities/request)
;;; request.el ends here
