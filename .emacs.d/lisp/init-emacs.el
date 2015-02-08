;;; init-emacs.el --- initializes basic Emacs settings
;;; Commentary:
;;; Code:

(defconst *user-custom-file* (path-join *user-data-directory* "custom.el"))

;; Create data and cache directories
(make-directory *user-cache-directory* t)
(make-directory *user-data-directory* t)

(setq-default
 ;; Lines of history in the message buffer.
 message-log-max 10000
 ;; Reduce number of pauses due to garbage collection.
 gc-cons-threshold (* 50 1024 1024)
 gc-cons-percentage 0.5
 ;; Path to custom-file
 custom-file *user-custom-file*)


(provide 'init-emacs)
;;; init-emacs.el ends here
