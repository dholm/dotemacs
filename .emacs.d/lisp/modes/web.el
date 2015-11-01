;;; web.el --- Web development
;;; Commentary:
;;; Code:

(defun user/web-mode-hook ()
  "Web mode hook."
  (setq
   ;; Don't indent using tabs by default.
   indent-tabs-mode nil)

  (when (user/company-mode-p)
    (with-feature 'company-web
      (add-company-sources 'company-web-html)))

  (with-feature 'pandoc-mode
    (pandoc-mode t))

  (with-feature 'guide-key
    (guide-key/add-local-highlight-command-regexp "web-mode-")))


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

  (when (feature-p 'polymode)
    (add-auto-mode 'poly-javascript-erb-mode "\\.js\\.erb$")
    (add-auto-mode 'poly-coffee-erb-mode "\\.coffee\\.erb$")
    (add-auto-mode 'poly-html-erb-mode "\\.html\\.erb$"))

  (add-auto-mode 'web-mode "\\.html?$" "\\.phtml$" "\\.php[3-5]?$")

  ;;; (Hooks) ;;;
  (add-hook 'web-mode-hook 'user/web-mode-hook))


(defun user/tern-init ()
  "Initialize tern."
  (add-hook 'tern-mode-hook 'user/tern-mode-hook))


(defun user/ac-html-init ()
  "Initialize ac-html."
  (after-load 'web-mode
    (add-to-list 'web-mode-ac-sources-alist
                 '("html" . (ac-source-html-attribute-value
                             ac-source-html-tag
                             ac-source-html-attribute)))))


(defun user/web-init ()
  "Initialize web development."
  (require-package '(:name web-mode :after (user/web-mode-init)))
  (with-executable 'npm
    (require-package '(:name tern :after (user/tern-init)))
    (require-package '(:name company-tern)))
  (require-package '(:name ac-html :after (user/ac-html-init)))
  (require-package '(:name company-web))
  (require-package '(:name skewer-mode))
  (require-package '(:name tidy)))

(user/web-init)


(provide 'modes/web)
;;; web.el ends here
