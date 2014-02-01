;;; outline.el --- Outline mode support
;;; Commentary:
;;; Code:

(defun user/outline-mode-hook ()
  "Hook for outline mode."
  ;;; (Bindings) ;;;
  (when (el-get-package-is-installed 'outline-magic)
    (define-key user/view-map (kbd "f") 'outline-cycle)
    (user/bind-key-local :navigation :context-up 'outline-move-subtree-up)
    (user/bind-key-local :navigation :context-down 'outline-move-subtree-down)
    (user/bind-key-local :code :context-promote 'outline-promote)
    (user/bind-key-local :code :context-demote 'outline-demote))
  (user/bind-key-local :navigation :context-forward 'outline-next-heading)
  (user/bind-key-local :navigation :context-backward 'outline-previous-heading))


(defun user/outline-mode-init ()
  "Initialize outline mode."
  (add-hook 'outline-mode-hook 'user/outline-mode-hook)
  (add-hook 'outline-minor-mode-hook 'user/outline-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name outline-magic)))

(user/outline-mode-init)


(provide 'utilities/outline)
;;; outline.el ends here
