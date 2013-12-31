;;; outline.el --- Outline mode support
;;; Commentary:
;;; Code:

(defun user/outline-mode-hook ()
  "Hook for outline mode."
  ;;; (Bindings) ;;;
  (when (el-get-package-is-installed 'outline-magic)
    (define-key user/view-map (kbd "f") 'outline-cycle)
    (define-key user/code-map (kbd "p") 'outline-move-subtree-up)
    (define-key user/code-map (kbd "n") 'outline-move-subtree-down)
    (define-key user/code-map (kbd "P") 'outline-promote)
    (define-key user/code-map (kbd "N") 'outline-demote))
  (define-key user/navigation-map (kbd "p") 'outline-previous-heading)
  (define-key user/navigation-map (kbd "n") 'outline-next-heading))


(defun user/outline-mode-init ()
  "Initialize outline mode."
  (add-hook 'outline-mode-hook 'user/outline-mode-hook)
  (add-hook 'outline-minor-mode-hook 'user/outline-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name outline-magic)))

(user/outline-mode-init)


(provide 'utilities/outline)
;;; outline.el ends here
