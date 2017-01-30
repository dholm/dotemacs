;;; css --- initializes CSS modes
;;; Commentary:
;;; Code:

(defun user--css-mode-hook ()
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


(defun user--css-mode-config ()
  "Initialize CSS mode."
  (add-hook 'css-mode-hook 'user--css-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name showcss-mode))
  (use-package rainbow-mode
    :ensure t)
  (when (feature-p 'helm)
    (use-package helm-css-scss
      :ensure t)))

(user--css-mode-config)


(provide 'modes/css)
;;; css.el ends here
