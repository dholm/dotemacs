;;; backup --- Emacs backup system
;;; Commentary:
;;; Code:
(defconst *user-backup-directory* (path-join *user-cache-directory* "backups"))

(setq
 ;; Put backups in the cache directory
 backup-directory-alist `((".*" . ,*user-backup-directory*))
 ;; Version-control backup files
 version-control t
 ;; Keep 16 new versions and 2 old versions
 kept-new-versions 6
 kept-old-versions 2
 ;; Delete old versions without asking
 delete-old-versions t
 ;; Always backup by copy
 backup-by-copying t)


(provide 'ux/backup)
;;; backup.el ends here
