;;; help.el --- Emacs help
;;; Commentary:
;;; Code:

(use-package faces
  :ensure nil
  :init
  (user/bind-key-global :emacs :describe-face 'describe-face)
  (user/bind-key-global :emacs :describe-all-faces 'list-faces-display))

(use-package help-fns
  :ensure nil
  :init
  (user/bind-key-global :emacs :describe-function 'describe-function)
  (user/bind-key-global :emacs :describe-variable 'describe-variable)
  (user/bind-key-global :emacs :describe-syntax 'describe-syntax))

(use-package info-look
  :ensure nil
  :init
  (user/bind-key-global :emacs :describe-symbol 'info-lookup-symbol))

(use-package help
  :ensure nil
  :init
  (user/bind-key-global :emacs :describe-bindings 'describe-bindings)
  (user/bind-key-global :emacs :describe-key 'describe-key)
  (user/bind-key-global :emacs :where-is 'where-is)
  (user/bind-key-global :emacs :describe-mode 'describe-mode))

(use-package find-func
  :ensure nil
  :init
  (user/bind-key-global :emacs :find-library 'find-library))

(use-package finder
  :ensure nil
  :init
  (user/bind-key-global :emacs :find-package 'finder-by-keyword))

(use-package menu-bar
  :ensure nil
  :init
  (user/bind-key-global :emacs :elisp-search 'elisp-index-search))

(use-package tutorial
  :ensure nil
  :init
  (user/bind-key-global :emacs :tutorial 'help-with-tutorial))


(use-package guide-key
  :diminish guide-key-mode
  :config
  (validate-setq
   ;; Register all the prefix keys.
   guide-key/recursive-key-sequence-flag t
   guide-key/guide-key-sequence user/prefix-list
   ;; Number of seconds until help is shown.
   guide-key/idle-delay 1.0)

  ;; Enable guide key.
  (guide-key-mode t)

  (use-package guide-key-tip
    :if window-system
    :config
    (validate-setq
     ;; Tooltips should only be used in graphical mode.
     guide-key-tip/enabled t)))


(provide 'ux/help)
;;; help.el ends here
