;;; help.el --- Emacs help -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package faces
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :describe-face 'describe-face)
  (user/bind-key-global :emacs :describe-all-faces 'list-faces-display))

(use-package help-mode
  :ensure nil
  :defer
  :config
  (use-package help-fns
    :ensure nil
    :init
    (user/bind-key-global :emacs :describe-variable 'describe-variable)
    (user/bind-key-global :emacs :describe-syntax 'describe-syntax))

  (use-package helpful
    :init
    (user/bind-key-global :emacs :describe-function 'helpful-function)
    (user/bind-key-global :emacs :describe-macro 'helpful-macro)
    (user/bind-key-global :emacs :describe-command 'helpful-command)
    :config
    (with-eval-after-load 'popwin
      ;; Use popwin for helpful buffer.
      (add-to-list
       'popwin:special-display-config
       ;; Don't select compilation window when shown
       '(helpful-mode :position bottom :height 20)))))

(use-package apropos
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :search-variable-value 'apropos-value))

(use-package info-look
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :describe-symbol 'info-lookup-symbol))

(use-package help
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :describe-bindings 'describe-bindings)
  (user/bind-key-global :emacs :describe-key 'describe-key)
  (user/bind-key-global :emacs :where-is 'where-is)
  (user/bind-key-global :emacs :describe-mode 'describe-mode))

(use-package find-func
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :find-library 'find-library))

(use-package finder
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :find-package 'finder-by-keyword))

(use-package menu-bar
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :elisp-search 'elisp-index-search))

(use-package tutorial
  :ensure nil
  :defer
  :init
  (user/bind-key-global :emacs :tutorial 'help-with-tutorial))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode t)
  :config
  (validate-setq
   ;; Number of seconds until help is shown.
   which-key-idle-delay 1.0
   which-key-special-keys
   '("SPC"
     "TAB"
     "RET"
     "DLT" ; delete key
     "BS" ; backspace key
     "ESC"))

  (when (eq default-terminal-coding-system 'utf-8)
    (add-many-to-list
     'which-key-replacement-alist
     '(("TAB" . nil)        . ("↹" . nil))
     '(("RET" . nil)        . ("⏎" . nil))
     '(("DEL" . nil)        . ("⌫" . nil))
     '(("deletechar" . nil) . ("⌦" . nil))
     '(("DEL" . nil)        . ("⇤" . nil))
     '(("SPC" . nil)        . ("␣" . nil))))

  (which-key-add-key-based-replacements
    user/view-prefix          "view"
    user/help-prefix          "help"
    user/documentation-prefix "doc"
    user/code-prefix          "code"
    user/code-eval-prefix     "eval"
    user/vcs-prefix           "vcs"
    user/utilities-prefix     "utils"
    user/apps-prefix          "apps"))


(provide 'ux/help)
;;; help.el ends here
