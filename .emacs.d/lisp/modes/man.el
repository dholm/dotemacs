;;; man.el --- Man page support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-man-cache-directory*
  (path-join *user-cache-directory* "man")
  "Path to user's man cache store.")

(use-package man
  :if (executable-find "man")
  :defer
  :bind-wrap
  (:map Man-mode-map
   ((:key :nav :functions/toc) . helm-imenu))
  :config
  (validate-setq
   ;; Make man-mode wrap lines at half the width of Emacs.
   Man-width (/ (window-total-width (frame-root-window)) 2)))

(use-package woman
  :if (executable-find "man")
  :defer
  :bind-wrap
  (:map woman-mode-map
   ((:key :nav :functions/toc) . helm-imenu))
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
    (user/bind-key-global :doc :manual 'man)))

(use-package helm-man
  :if (executable-find "man")
  :ensure helm
  :config
  ;; Validation won't work because because the supported functions
  ;; have been hardcoded in the option list.
  (setq
   helm-man-or-woman-function 'WoMan-getpage-in-background))

(provide 'modes/man)
;;; man.el ends here
