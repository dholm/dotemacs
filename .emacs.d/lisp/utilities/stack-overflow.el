;;; stack-overflow.el --- Stack Overflow search support.
;;; Commentary:
;;; Code:

(defun user--stack-overflow-prog-mode-hook ()
  "Prog mode hook for Stack Overflow."
  ;;; (Bindings) ;;;
  (when (feature-p 'emacs-sos)
    (user/bind-key-local :util :stack-overflow-search 'sos)))


(use-package sos
  :defer t
  :init
  (add-hook 'prog-mode-hook 'user--stack-overflow-prog-mode-hook))


(provide 'utilities/stack-overflow)
;;; stack-overflow.el ends here
