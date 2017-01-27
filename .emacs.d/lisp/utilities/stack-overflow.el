;;; stack-overflow.el --- Stack Overflow search support.
;;; Commentary:
;;; Code:

(defun user/stack-overflow-prog-mode-hook ()
  "Prog mode hook for Stack Overflow."
  ;;; (Bindings) ;;;
  (when (el-get-package-is-installed 'emacs-sos)
    (user/bind-key-local :util :stack-overflow-search 'sos)))


(defun user/stack-overflow-init ()
  "Initialize Stack Overflow support."
  (add-hook 'prog-mode-hook 'user/stack-overflow-prog-mode-hook)

  ;;; (Packages) ;;;
  (req-package emacs-sos
    :loader :el-get))

(user/stack-overflow-init)


(provide 'utilities/stack-overflow)
;;; stack-overflow.el ends here
