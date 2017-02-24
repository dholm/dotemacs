;;; help.el --- Emacs help
;;; Commentary:
;;; Code:

(defun user--guide-key-config ()
  "Initialize guide key."
  (validate-setq
   ;; Register all the prefix keys.
   guide-key/recursive-key-sequence-flag t
   guide-key/guide-key-sequence user/prefix-list
   ;; Number of seconds until help is shown.
   guide-key/idle-delay 1.0)

  ;; Enable guide key.
  (guide-key-mode t))


(defun user--help-config ()
  "Initialize Emacs help."
  (user/bind-key-global :emacs :describe-face 'describe-face)
  (user/bind-key-global :emacs :describe-all-faces 'list-faces-display)
  (user/bind-key-global :emacs :describe-function 'describe-function)
  (user/bind-key-global :emacs :describe-variable 'describe-variable)
  (user/bind-key-global :emacs :describe-symbol 'info-lookup-symbol)

  (user/bind-key-global :emacs :describe-bindings 'describe-bindings)
  (user/bind-key-global :emacs :describe-key 'describe-key)
  (user/bind-key-global :emacs :describe-key-extensive
                        'Info-goto-emacs-key-command-node)
  (user/bind-key-global :emacs :where-is 'where-is)

  (user/bind-key-global :emacs :find-library 'find-library)
  (user/bind-key-global :emacs :find-package 'finder-by-keyword)
  (user/bind-key-global :emacs :manual 'info-emacs-manual)
  (user/bind-key-global :emacs :elisp-search 'elisp-index-search)

  (user/bind-key-global :emacs :describe-mode 'describe-mode)
  (user/bind-key-global :emacs :describe-syntax 'describe-syntax)

  (user/bind-key-global :emacs :tutorial 'help-with-tutorial)

  ;;; (Packages) ;;;
  (use-package guide-key
    :diminish guide-key-mode
    :config (user--guide-key-config))

  (use-package guide-key-tip
    :ensure guide-key
    :if window-system
    :config
    (validate-setq
     ;; Tooltips should only be used in graphical mode.
     guide-key-tip/enabled t)))

(user--help-config)


(provide 'ux/help)
;;; help.el ends here
