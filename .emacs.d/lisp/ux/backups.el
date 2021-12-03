;;; backups.el --- Emacs backup system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Set up the backups directory.
(defconst *user-backup-directory* (path-join *user-cache-directory* "backups"))
;; Set up the autosaves directory.
(defconst *user-auto-save-directory* (path-join *user-cache-directory* "auto-saves"))
;; Emacs will create the backup dir automatically, but not the autosaves dir.
(make-directory *user-auto-save-directory* t)


(defun user--backups-config ()
  "Initialize Emacs backup system."
  (validate-setq
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

  (validate-setq
   ;; Auto-save every minute or 300 events
   auto-save-interval 300
   auto-save-timeout 60
   ;; Always auto-save buffers.
   auto-save-default t
   ;; Put autosave files (ie #foo#) and backup files (ie foo~) into a cache dir.
   auto-save-file-name-transforms `((".*" ,(concat *user-auto-save-directory* "/\\1") t))
   ;; Put session backups into the cache directory.
   auto-save-list-file-prefix (path-join *user-auto-save-directory* ".saves-"))

  (use-package async-backup
    :hook (after-save-hook . async-backup)))


(user--backups-config)


(provide 'ux/backups)
;;; backups.el ends here
