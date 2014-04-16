;;; help.el --- Emacs help
;;; Commentary:
;;; Code:

(defun user/guide-key-init ()
  "Initialize guide key."
  (setq-default
   ;; Register all the prefix keys.
   guide-key/recursive-key-sequence-flag t
   guide-key/guide-key-sequence user/prefix-list
   ;; Number of seconds until help is shown.
   guide-key/idle-delay 1.0
   ;; Tooltips should only be used in graphical mode.
   guide-key-tip/enabled (display-graphic-p))

  ;; Enable guide key.
  (guide-key-mode t))


(defun user/help-init ()
  "Initialize Emacs help."
  (user/bind-key-global :emacs :describe-function 'describe-function)
  (user/bind-key-global :emacs :describe-variable 'describe-variable)
  (user/bind-key-global :emacs :describe-symbol 'info-lookup-symbol)

  (user/bind-key-global :emacs :describe-bindings 'describe-bindings)
  (user/bind-key-global :emacs :describe-key 'describe-key)
  (user/bind-key-global :emacs :describe-key-extensive
                        'Info-goto-emacs-key-command-node)
  (user/bind-key-global :emacs :where-is 'where-is)

  (user/bind-key-global :emacs :find-package 'finder-by-keyword)
  (user/bind-key-global :emacs :manual 'info-emacs-manual)

  (user/bind-key-global :emacs :describe-mode 'describe-mode)
  (user/bind-key-global :emacs :describe-syntax 'describe-syntax)

  (user/bind-key-global :emacs :tutorial 'help-with-tutorial)

  ;;; (Packages) ;;;
  (require-package '(:name guide-key :after (user/guide-key-init)))
  (when (display-graphic-p)
    (require-package '(:name guide-key-tip))))

(user/help-init)


(provide 'ux/help)
;;; help.el ends here
