;;; sessions.el --- Set up Emacs to remember things between sessions
;;; Commentary:
;;; Code:

(defun user--recentf-config ()
  "Initialize Emacs recent files history."
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude '("/tmp/")
   recentf-save-file (path-join *user-cache-directory* "recentf"))

  (after-load 'cedet-devel-load
    ;; Prevent entries from loading tramp resulting in the
    ;; stable version of CEDET being loaded before devel.
    (recentf-mode t)))


(defun user--savehist-config ()
  "Initialize Emacs save history."
  (setq-default
   savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
   savehist-file (path-join *user-cache-directory* "savehist"))

  (savehist-mode t))


(defun user--saveplace-config ()
  "Initialize Emacs buffer location history."
  (with-feature 'saveplace
    (setq-default
     ;; Location of saveplace cache store.
     save-place-file (path-join *user-cache-directory* "saveplace")
     ;; Enable.
     save-place t)))


(defun user--sessions-config ()
  "Initialize Emacs session management."
  (user--recentf-config)
  (user--savehist-config)
  (user--saveplace-config))

(user--sessions-config)


(provide 'ux/sessions)
;;; sessions.el ends here
