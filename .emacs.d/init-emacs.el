
;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)


;; Set up the autosaves directory
(defconst emacs-autosaves-directory (path-join *user-cache-directory* "autosaves"))
;; Emacs will create the backup dir automatically, but not the autosaves dir
(make-directory emacs-autosaves-directory t)


(setq-default
 ;; Redraw the entire screen before checking for pending input events.
 ;; This will improve performance in general but might degrade performance of
 ;; key repeat.
 redisplay-dont-pause t
 ;; Put autosave files (ie #foo#) and backup files (ie foo~) into a cache dir
 auto-save-file-name-transforms `((".*" ,(concat emacs-autosaves-directory "/\\1") t))
 backup-directory-alist `((".*" . ,(path-join *user-cache-directory* "backups")))
 ;; Put session backups into the cache directory
 auto-save-list-file-prefix (path-join *user-cache-directory* "auto-save-list" ".saves-"))


(provide 'init-emacs)
