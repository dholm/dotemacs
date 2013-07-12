
;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)


;; Set up the autosaves directory
(defconst emacs-autosaves-directory (path-join *user-cache-directory* "autosaves"))
;; Emacs will create the backup dir automatically, but not the autosaves dir
(make-directory emacs-autosaves-directory t)


(setq-default
 ;; Do not show the splash screen or message
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 ;; Inhibit GUI features
 use-file-dialog nil
 user-dialog-box nil
 ;; Redraw the entire screen before checking for pending input events.
 ;; This will improve performance in general but might degrade performance of
 ;; key repeat.
 redisplay-dont-pause t
 ;; Show row and column numbers
 line-number-mode t
 column-number-mode t
 ;; Put autosave files (ie #foo#) and backup files (ie foo~) into a cache dir
 auto-save-file-name-transforms `((".*" ,(concat emacs-autosaves-directory "/\\1") t))
 backup-directory-alist `((".*" . ,(path-join *user-cache-directory* "backups")))
 ;; Put session backups into the cache directory
 auto-save-list-file-prefix (path-join *user-cache-directory* "auto-save-list" ".saves-"))


;; Display the current time and system load
(require 'time)
(setq display-time-24hr-format t
      display-time-form-list (list 'time 'load)
      display-time-day-and-date t)
(display-time)


;; Enable blinking cursor
(blink-cursor-mode)
