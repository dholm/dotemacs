;;; annotate.el --- Annotate any file without touching it
;;; Commentary:
;;; Code:

(defconst *user-annotations-file*
  (path-join *user-data-directory* "annotations")
  "Path to user's Gnus data store.")

(use-package annotate
  :config
  (validate-setq
   ;; Set path to where annotations are stored.
   annotate-file *user-annotations-file*
   ;; Disable status messages.
   annotate-use-messages nil))


(provide 'utilities/annotate)
;;; annotate.el ends here
