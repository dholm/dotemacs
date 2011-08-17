;; Do not show the splash screen or message
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)


;; Put autosave files (ie #foo#) and backup files (ie foo~) into a cache dir
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.cache/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.cache/backups/"))))

;; Emacs will create the backup dir automatically, but not the autosaves dir
(make-directory "~/.emacs.cache/autosaves/" t)


;; Put session backups into the cache directory
(setq auto-save-list-file-prefix "~/.emacs.cache/auto-save-list/.saves-")


;; Default to UTF-8
(prefer-coding-system 'utf-8)


;; Properly display colors in shell
(ansi-color-for-comint-mode-on)
