;;; sessions.el --- Set up Emacs to remember things between sessions
;;; Commentary:
;;; Code:

(defun user/recentf-init ()
  "Initialize Emacs recent files history."
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude '("/tmp/"
                     ;; Prevent entries from loading tramp resulting in the
                     ;; stable version of CEDET being loaded before devel.
                     "/ssh:" "/sudo:")
   recentf-save-file (path-join *user-cache-directory* "recentf"))

  (recentf-mode t))


(defun user/savehist-init ()
  "Initialize Emacs save history."
  (setq-default
   savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
   savehist-file (path-join *user-cache-directory* "savehist"))

  (savehist-mode t))


(defun user/saveplace-init ()
  "Initialize Emacs buffer location history."
  (with-feature 'saveplace
    (setq-default
     ;; Location of saveplace cache store.
     save-place-file (path-join *user-cache-directory* "saveplace")
     ;; Enable.
     save-place t)))


(defun user/sessions-init ()
  "Initialize Emacs session management."
  (user/recentf-init)
  (user/savehist-init)
  (user/saveplace-init))

(user/sessions-init)


(provide 'ux/sessions)
;;; sessions.el ends here
