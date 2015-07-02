;;; annotate.el --- Annotate any file without touching it
;;; Commentary:
;;; Code:

(defconst *user-annotations-file*
  (path-join *user-data-directory* "annotations")
  "Path to user's Gnus data store.")

(defun user/annotate.el-init ()
  "Initialize annotate.el."
  (setq-default
   ;; Set path to where annotations are stored.
   annotate-file *user-annotations-file*
   ;; Disable status messages.
   annotate-use-messages nil))

(require-package '(:name annotate.el :after (user/annotate.el-init)))


(provide 'utilities/annotate)
;;; annotate.el ends here
