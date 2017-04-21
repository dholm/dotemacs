;;; sessions.el --- Set up Emacs to remember things between sessions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :init
  ;; Prevent entries from loading tramp resulting in the stable
  ;; version of CEDET being loaded before devel.
  (add-hook 'user--after-init-hook 'recentf-mode t)
  :config
  (validate-setq
   recentf-max-saved-items 1000
   recentf-exclude '("/elpa/" "/tmp/")
   recentf-save-file (path-join *user-cache-directory* "recentf")))

(use-package savehist
  :init
  ;; Must be set before enabling savehist so validate cannot be used
  ;; here.
  (setq-default
   savehist-file (path-join *user-cache-directory* "savehist")
   ;; Save minibuffer history.
   savehist-save-minibuffer-history t
   ;; Additional history to save.
   savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
   ;; Autosave every once in a while.
   savehist-autosave-interval 180)

  (savehist-mode t))

(use-package saveplace
  :config
  (validate-setq
   ;; Location of saveplace cache store.
   save-place-file (path-join *user-cache-directory* "saveplace")
   ;; Enable.
   save-place-mode t))


(provide 'ux/sessions)
;;; sessions.el ends here
