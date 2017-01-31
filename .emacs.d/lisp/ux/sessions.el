;;; sessions.el --- Set up Emacs to remember things between sessions
;;; Commentary:
;;; Code:

(defun user--recentf-config ()
  "Initialize Emacs recent files history."
  (validate-setq
   recentf-max-saved-items 1000
   recentf-exclude '("/tmp/")
   recentf-save-file (path-join *user-cache-directory* "recentf"))

  ;; Prevent entries from loading tramp resulting in the stable
  ;; version of CEDET being loaded before devel.
  (add-hook 'user--after-init-hook 'recentf-mode t))


(defun user--savehist-config ()
  "Initialize Emacs save history."
  (validate-setq
   savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
   savehist-file (path-join *user-cache-directory* "savehist"))

  (savehist-mode t))


(defun user--saveplace-config ()
  "Initialize Emacs buffer location history."
  (validate-setq
   ;; Location of saveplace cache store.
   save-place-file (path-join *user-cache-directory* "saveplace")
   ;; Enable.
   save-place t))


(use-package recentf
  :ensure t
  :config (user--recentf-config))
(use-package savehist
  :ensure t
  :config (user--savehist-config))
(use-package saveplace
  :ensure t
  :config (user--saveplace-config))


(provide 'ux/sessions)
;;; sessions.el ends here
