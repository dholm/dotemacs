;;; bookmark+ --- initializes Bookmark+ extension
;;; Commentary:
;;; Code:

(defconst *bookmark+-data-file* (path-join *user-data-directory* "bookmarks"))
(defconst *bookmark+-menu-state-file* (path-join *user-cache-directory* "bookmark-menu-state.el"))


(defun dholm/bookmark+-before-init ()
  "Setup before loading bookmark+."
  (setq-default
   ;; Enable versioned backups
   bookmark-version-control t
   ;; Save bookmarks after ten updates
   bmkp-count-multi-mods-as-one-flag t
   bookmark-save-flag 1
   ;; Put the menu state in the cache directory
   bmkp-bmenu-state-file *bookmark+-menu-state-file*
   ;; Put the repository in the data directory
   bookmark-default-file *bookmark+-data-file*))

(defun dholm/bookmark+-init ()
  "Initialize bookmark+.")

(require-package '(:name bookmark+
                         :before (dholm/bookmark+-before-init)
                         :after (dholm/bookmark+-init)))


(provide 'utilities/bookmark+)
;;; bookmark+.el ends here
