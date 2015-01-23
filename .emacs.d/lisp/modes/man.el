;;; man.el --- Man page support
;;; Commentary:
;;; Code:

(defconst *user-man-cache-directory*
 (path-join *user-cache-directory* "man")
 "Path to user's man cache store.")


(defun user/woman-init ()
  "Initialize Emacs WoMan."
  (setq-default
   ;; WoMan cache store.
   woman-cache-file-name (path-join *user-man-cache-directory* "wmcache.el")
   ;; Use highest cache level.
   woman-cache-level 3
   ;; Have WoMan fill the entire frame.
   woman-fill-frame t)

  ;;; (Bindings) ;;;
  (user/bind-key-global :doc :manual 'woman))


(defun user/man-mode-init ()
  "Initialize Emacs man-mode."
  (setq-default
   ;; Make man-mode wrap lines at half the width of Emacs.
   Man-width (/ (window-total-width (frame-root-window)) 2)))


(defun user/man-init ()
  "Initialize Emacs man support."
  (user/man-mode-init)
  (user/woman-init)

  (make-directory *user-man-cache-directory* t))

(with-executable 'man
  (user/man-init))


(provide 'modes/man)
;;; man.el ends here
