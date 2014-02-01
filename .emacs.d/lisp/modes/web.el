;;; web.el --- Web development
;;; Commentary:
;;; Code:

(defun user/web-mode-hook ()
  "Web mode hook.")


(defun user/tern-mode-hook ()
  "Tern mode hook."
  (tern-ac-setup)

  ;;; (Bindings) ;;;
  (user/bind-key-local :navigation :follow-symbol 'tern-find-definition)
  (user/bind-key-local :navigation :go-back 'tern-pop-find-definition)
  (user/bind-key-local :docs :reference 'tern-get-docs))


(defun user/web-mode-init ()
  "Initialize web mode."
  (setq-default
   ;; Indent HTML automatically.
   web-mode-indent-style 2
   ;; Indentation offsets.
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   ;; Highlight current HTML element.
   web-mode-enable-current-element-highlight t)

  (add-hook 'web-mode-hook 'user/web-mode-hook)

  (add-auto-mode 'web-mode "\\.html?$" "\\.phtml$" "\\.php[34]?$"))


(defun user/tern-init ()
  "Initialize tern."
  (add-hook 'tern-mode-hook 'user/tern-mode-hook))


(defun user/web-init ()
  "Initialize web development."
  (require-package '(:name web-mode :after (user/web-mode-init)))
  (require-package '(:name tern :after (user/tern-init)))
  (require-package '(:name skewer-mode))
  (require-package '(:name tidy)))

(user/web-init)


(provide 'modes/web)
;;; web.el ends here
