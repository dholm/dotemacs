;;; web.el --- Web development
;;; Commentary:
;;; Code:

(defun user/web-mode-hook ()
  "Web mode hook."
  (with-feature 'pandoc-mode
    (pandoc-mode t)))


(defmacro user/add-web-mode-hook (engine function)
  "In web mode hook, when using ENGINE, evaluate FUNCTION."
  `(add-hook 'web-mode-hook
             (lambda ()
               (when (string= web-mode-engine (symbol-name ,engine))
                 (funcall ,function)))))


(defun user/tern-mode-hook ()
  "Tern mode hook."
  (tern-ac-setup)

  ;;; (Bindings) ;;;
  (user/bind-key-local :nav :follow-symbol 'tern-find-definition)
  (user/bind-key-local :nav :go-back 'tern-pop-find-definition)
  (user/bind-key-local :doc :reference 'tern-get-docs))


(defun user/current-buffer-django-p ()
  "Evaluate to non-nil if the current buffer should use the `django` engine."
  (save-excursion
    (search-forward-regexp (concat "{% base\\|{% if\\|{% for\\|{% include\\|"
                                   "{% block\\|{% csrf_token %}") nil t)))


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
   web-mode-enable-current-element-highlight t
   ;; Engine selection.
   web-mode-engines-alist
   '(("django" . user/current-buffer-django-p)
     ("php" . "\\.php[3-5]?")))

  (add-hook 'web-mode-hook 'user/web-mode-hook)

  (add-auto-mode 'web-mode "\\.html?$" "\\.phtml$" "\\.php[3-5]?$"))


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
