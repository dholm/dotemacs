(require-package
 (:name bookmark+
        :before (progn
                  (setq
                   ;; Enable versioned backups
                   bookmark-version-control t
                   ;; Save bookmarks after ten updates
                   bmkp-count-multi-mods-as-one-flag t
                   bookmark-save-flag 10
                   ;; Put the repository in the data directory
                   bookmark-default-file (path-join *user-data-directory* "bookmarks"))
                  )))


(provide 'utilities/bookmark+)
