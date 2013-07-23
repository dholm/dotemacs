;;; auto-save --- initializes Emacs auto-saves
;;; Commentary:
;;; Code:

;; Set up the autosaves directory
(defconst *user-auto-save-directory* (path-join *user-cache-directory* "auto-saves"))

;; Emacs will create the backup dir automatically, but not the autosaves dir
(make-directory *user-auto-save-directory* t)


(setq
 ;; Put autosave files (ie #foo#) and backup files (ie foo~) into a cache dir
 auto-save-file-name-transforms `((".*" ,(concat *user-auto-save-directory* "/\\1") t))
 ;; Put session backups into the cache directory
 auto-save-list-file-prefix (path-join *user-auto-save-directory* ".saves-"))


(provide 'ux/auto-save)
;;; auto-save.el ends here
