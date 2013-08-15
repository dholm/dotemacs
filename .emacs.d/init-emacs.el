;;; init-emacs.el --- initializes basic Emacs settings
;;; Commentary:
;;; Code:


(defconst *user-custom-file* (path-join *user-data-directory* "custom.el"))

;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)


(setq-default
 ;; When using fill-paragraph or auto-fill-mode break lines at 80 characters by
 ;; default
 fill-column 80
 ;; Highlight matches when using grep
 grep-highlight-matches t
 ;; Redraw the entire screen before checking for pending input events.
 ;; This will improve performance in general but might degrade performance of
 ;; key repeat.
 redisplay-dont-pause t
 ;; Path to custom-file
 custom-file *user-custom-file*)


(provide 'init-emacs)
;;; init-emacs.el ends here
