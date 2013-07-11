;;; (Initialization) ;;;


(setq-default
 ;; Enable versioned backups
 bookmark-version-control t
 ;; Save bookmarks on every update
 bmkp-count-multi-mods-as-one-flag t
 bookmark-save-flag 1
 ;; Put the repository in the data directory
 bookmark-default-file (path-join *user-data-directory* "bookmarks"))
