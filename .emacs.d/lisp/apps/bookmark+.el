;;; bookmark+.el --- initializes Bookmark+ extension
;;; Commentary:
;;; Code:

(defconst *bookmark+-data-file* (path-join *user-data-directory* "bookmarks"))
(defconst *bookmark+-menu-state-file* (path-join *user-cache-directory* "bookmark-menu-state.el"))


(defun user/bookmark+-before-init ()
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


(defun user/bookmark+-init ()
  "Initialize bookmark+."
  ;;; (Bindings) ;;;
  ;; Bind bookmarks to C-c b
  (global-set-key (kbd "C-c b") 'bookmark-map)

  (define-key bookmark-map (kbd "l") 'bookmark-jump)
  (define-key bookmark-map (kbd "e") 'bmkp-edit-bookmark-record)
  (define-key bookmark-map (kbd "t") 'bmkp-add-tags))


(require-package '(:name bookmark+
                         :before (user/bookmark+-before-init)
                         :after (user/bookmark+-init)))


(provide 'apps/bookmark+)
;;; bookmark+.el ends here
