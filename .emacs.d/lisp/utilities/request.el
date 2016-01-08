;;; request.el --- interface to The Silver Searcher
;;; Commentary:
;;; Code:

(defun user/request-init ()
  "Initialize request."
  (setq-default
   ;; Request cache store.
   request-storage-directory (path-join *user-cache-directory* "request")))

(require-package '(:name request :after (user/request-init)))


(provide 'utilities/request)
;;; request.el ends here
