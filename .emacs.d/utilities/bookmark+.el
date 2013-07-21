(require-package '(:name bookmark+ :before (dholm/bookmark+-before-init)))


(defconst *bookmark+-data-file* (path-join *user-data-directory* "bookmarks"))


(defun dholm/bookmark+-before-init ()
  (setq-default
   ;; Enable versioned backups
   bookmark-version-control t
   ;; Save bookmarks after ten updates
   bmkp-count-multi-mods-as-one-flag t
   bookmark-save-flag 1
   ;; Put the repository in the data directory
   bookmark-default-file *bookmark+-data-file*))


(provide 'utilities/bookmark+)
