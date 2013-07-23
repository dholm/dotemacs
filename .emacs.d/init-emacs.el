;;; init-emacs --- initializes basic Emacs settings
;;; Commentary:
;;; Code:

;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)


(setq-default
 ;; Redraw the entire screen before checking for pending input events.
 ;; This will improve performance in general but might degrade performance of
 ;; key repeat.
 redisplay-dont-pause t
 ;; Path to custom-file
 custom-file *user-custom-file*)


(provide 'init-emacs)
;;; init-emacs.el ends here
