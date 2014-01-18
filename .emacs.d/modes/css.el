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
  (rainbow-mode t))


(defun user/css-mode-init ()
  "Initialize CSS mode."
  (require-package '(:name showcss-mode))

  (add-hook 'css-mode-hook 'user/css-mode-hook))

(user/css-mode-init)


(provide 'modes/css)
;;; css.el ends here
