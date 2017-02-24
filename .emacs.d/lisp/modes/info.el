;;; info.el --- Info mode.
;;; Commentary:
;;; Code:

(defun user--info-mode-hook ()
  "Info mode hook."
  (when (feature-p 'info+)
    (require 'info+))

  (user/bind-key-local :nav :go-forward 'Info-history-forward)
  (user/bind-key-local :nav :go-back 'Info-history-back))

(use-package info
  :defer t
  :init
  (add-hook 'Info-mode-hook 'user--info-mode-hook)
  :config
  ;;; (Packages) ;;;
  (use-package info+
    :defer t
    :config
    (validate-setq
     ;; Enable breadcrumbs in header line.
     Info-breadcrumbs-in-header-flag t
     Info-breadcrumbs-in-mode-line-mode nil))

  (use-package helm-info
    :defer t
    :ensure helm
    :bind (("C-c h h e" . helm-info-emacs)
           ("C-c h h i" . helm-info-at-point))))


(provide 'modes/info)
;;; info.el ends here
