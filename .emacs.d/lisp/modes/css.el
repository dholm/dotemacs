;;; css --- initializes CSS modes
;;; Commentary:
;;; Code:

(defun user/css-mode-hook ()
  "CSS mode hook."
  (setq-default
   cssm-indent-function #'cssm-c-style-indenter
   cssm-indent-level '4)

  ;; Initialize auto completion
  (tern-ac-setup)
  (ac-css-mode-setup)

  (showcss-mode t)
  (skewer-css-mode t)
  (rainbow-mode t)

  ;;; (Bindings) ;;;
  (user/bind-key-local :nav :functions/toc 'helm-css-scss)
  (user/bind-key-local :nav :go-back 'helm-css-scss-back-to-last-point))


(defun user/css-mode-init ()
  "Initialize CSS mode."
  (add-hook 'css-mode-hook 'user/css-mode-hook)

  ;;; (Packages) ;;;
  (req-package showcss-mode
    :loader :el-get)
  (req-package rainbow-mode)
  (when (feature-p 'helm)
    (req-package helm-css-scss)))

(user/css-mode-init)


(provide 'modes/css)
;;; css.el ends here
