;; Set up the autosaves directory
(defconst *user-autosaves-directory* (path-join *user-cache-directory* "autosaves"))

;; Emacs will create the backup dir automatically, but not the autosaves dir
(make-directory *user-autosaves-directory* t)


(setq
 ;; Put autosave files (ie #foo#) and backup files (ie foo~) into a cache dir
 auto-save-file-name-transforms `((".*" ,(concat *user-autosaves-directory* "/\\1") t))
 backup-directory-alist `((".*" . ,(path-join *user-cache-directory* "backups")))
 ;; Put session backups into the cache directory
 auto-save-list-file-prefix (path-join *user-cache-directory* "auto-save-list" ".saves-")

 ;; Version-control backup files
 version-control t
 ;; Keep 16 new versions and 2 old versions
 kept-new-versions 6
 kept-old-versions 2
 ;; Delete old versions without asking
 delete-old-versions t
 ;; Always backup by copy
 backup-by-copying t)


(provide 'ux/auto-saves)
