;;; css --- initializes CSS modes
;;; Commentary:
;;; Code:

(defun user--css-mode-hook ()
  "CSS mode hook."
  (validate-setq
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


(use-package css-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'user--css-mode-hook)
  :config
  ;;; (Packages) ;;;
  (use-package showcss-mode
    :quelpa (showcss-mode
             :fetcher github
             :repo "smmcg/showcss-mode"))
  (use-package rainbow-mode)
  (when (feature-p 'helm)
    (use-package helm-css-scss)))


(provide 'modes/css)
;;; css.el ends here
