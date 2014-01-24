;;; sessions.el --- Set up Emacs to remember things between sessions
;;; Commentary:
;;; Code:

(defun user/sessions-init ()
  "Initialize Emacs session management."
  (user/recentf-init)
  (user/savehist-init)
  (user/saveplace-init)

  (require-package '(:name session :after (user/session-init))))


(defun user/recentf-init ()
  "Initialize Emacs recent files history."
  (setq-default
   recentf-max-saved-items 1000
   recentf-exclude '("/tmp/" "/ssh:")
   recentf-save-file (path-join *user-cache-directory* "recentf"))
  (recentf-mode t))


(defun user/savehist-init ()
  "Initialize Emacs save history."
  (require 'savehist)
  (setq-default
   savehist-additional-variables '(search-ring regexp-search-ring kill-ring)
   savehist-file (path-join *user-cache-directory* "savehist"))
  (savehist-mode t))


(defun user/saveplace-init ()
  "Initialize Emacs buffer location history."
  (require 'saveplace)
  (setq-default
   save-place-file (path-join *user-cache-directory* "saveplace")
   save-place t))


(defun user/session-init ()
  "Initialize session package."
  (setq-default
   session-save-file (path-join *user-cache-directory* "session")
   desktop-globals-to-save
   (append '((extended-command-history . 30)
             (file-name-history        . 100)
             (ido-last-directory-list  . 100)
             (ido-work-directory-list  . 100)
             (ido-work-file-list       . 100)
             (grep-history             . 30)
             (compile-history          . 30)
             (minibuffer-history       . 50)
             (query-replace-history    . 60)
             (read-expression-history  . 60)
             (regexp-history           . 60)
             (regexp-search-ring       . 20)
             (search-ring              . 20)
             (comint-input-ring        . 50)
             (shell-command-history    . 50)
             desktop-missing-file-warning
             tags-file-name
             register-alist)))

  (add-hook 'after-init-hook 'session-initialize))


(user/sessions-init)


(provide 'ux/sessions)
;;; sessions.el ends here
