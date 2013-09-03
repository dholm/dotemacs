;;; init-emacs.el --- initializes basic Emacs settings
;;; Commentary:
;;; Code:

(defconst *user-custom-file* (path-join *user-data-directory* "custom.el"))

;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)

(setq-default
 ;; Path to custom-file
 custom-file *user-custom-file*)


(provide 'init-emacs)
;;; init-emacs.el ends here
