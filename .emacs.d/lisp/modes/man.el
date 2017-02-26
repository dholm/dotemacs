;;; man.el --- Man page support
;;; Commentary:
;;; Code:

(defconst *user-man-cache-directory*
 (path-join *user-cache-directory* "man")
 "Path to user's man cache store.")

(with-executable 'man
  (use-package man
    :defer
    :config
    (validate-setq
     ;; Make man-mode wrap lines at half the width of Emacs.
     Man-width (/ (window-total-width (frame-root-window)) 2))

    (use-package helm-man
      :ensure helm))

  (use-package woman
    :defer
    :init
    (make-directory *user-man-cache-directory* t)
    :config
    (validate-setq
     ;; WoMan cache store.
     woman-cache-filename (path-join *user-man-cache-directory* "wmcache.el")
     ;; Use highest cache level.
     woman-cache-level 3
     ;; Have WoMan fill the entire frame.
     woman-fill-frame t)

    ;;; (Bindings) ;;;
    (if (feature-p 'helm)
        (user/bind-key-global :doc :manual 'helm-man-woman)
      (user/bind-key-global :doc :manual 'man))))


(provide 'modes/man)
;;; man.el ends here
