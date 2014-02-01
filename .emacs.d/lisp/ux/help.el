;;; help.el --- Emacs help
;;; Commentary:
;;; Code:

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

  (user/bind-key-global :emacs :tutorial 'help-with-tutorial))

(user/help-init)


(provide 'ux/help)
;;; help.el ends here
