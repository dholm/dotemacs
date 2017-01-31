;;; man.el --- Man page support
;;; Commentary:
;;; Code:

(defconst *user-man-cache-directory*
 (path-join *user-cache-directory* "man")
 "Path to user's man cache store.")


(defun user--woman-config ()
  "Initialize Emacs WoMan."
  (setq-default
   ;; WoMan cache store.
   woman-cache-file-name (path-join *user-man-cache-directory* "wmcache.el")
   ;; Use highest cache level.
   woman-cache-level 3
   ;; Have WoMan fill the entire frame.
   woman-fill-frame t)

  ;;; (Bindings) ;;;
  (if (feature-p 'helm)
      (user/bind-key-global :doc :manual 'helm-man-woman)
    (user/bind-key-global :doc :manual 'man)))


(defun user--man-mode-config ()
  "Initialize Emacs man-mode."
  (setq-default
   ;; Make man-mode wrap lines at half the width of Emacs.
   Man-width (/ (window-total-width (frame-root-window)) 2)))


(defun user--man-config ()
  "Initialize Emacs man support."
  (user--man-mode-config)
  (user--woman-config)

  (make-directory *user-man-cache-directory* t)

  ;;; (Packages) ;;;
  (when (feature-p 'helm)
    (use-package helm-man
      :ensure helm)))

(with-executable 'man
  (user--man-config))


(provide 'modes/man)
;;; man.el ends here
