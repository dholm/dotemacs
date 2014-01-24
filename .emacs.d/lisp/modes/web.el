;;; web.el --- Web development
;;; Commentary:
;;; Code:

(defun user/web-mode-hook ()
  "Web mode hook."
  (local-set-key (kbd "RET") 'newline-and-indent))


(defun user/tern-mode-hook ()
  "Tern mode hook."
  (define-key user/navigation-map (kbd "j") 'tern-find-definition)
  (define-key user/navigation-map (kbd "b") 'tern-pop-find-definition)
  (define-key user/documentation-map (kbd "d") 'tern-get-docs)
  (tern-ac-setup))


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
