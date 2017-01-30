;;; stack-overflow.el --- Stack Overflow search support.
;;; Commentary:
;;; Code:

(defun user--stack-overflow-prog-mode-hook ()
  "Prog mode hook for Stack Overflow."
  ;;; (Bindings) ;;;
  (when (feature-p 'emacs-sos)
    (user/bind-key-local :util :stack-overflow-search 'sos)))


(defun user--stack-overflow-config ()
  "Initialize Stack Overflow support."
  (add-hook 'prog-mode-hook 'user--stack-overflow-prog-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name emacs-sos)))

(user--stack-overflow-config)


(provide 'utilities/stack-overflow)
;;; stack-overflow.el ends here
