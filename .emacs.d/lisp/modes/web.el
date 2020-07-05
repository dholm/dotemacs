;;; web.el --- Web development -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--web-mode-hook ()
  "Web mode hook."
  (setq
   ;; Don't indent using tabs by default.
   indent-tabs-mode nil)

  (when (user/company-mode-p)
    (with-feature 'company-web
      (add-company-sources 'company-web-html)))

  (with-feature 'pandoc-mode
    (pandoc-mode t)))


(defmacro user--add-web-mode-hook (engine function)
  "In web mode hook, when using ENGINE, evaluate FUNCTION."
  `(add-hook 'web-mode-hook
             (lambda ()
               (when (string= web-mode-engine (symbol-name ,engine))
                 (funcall ,function)))))


(defun user--tern-mode-hook ()
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


(use-package web-mode
  :defer
  :mode "\.\(html?\|phtml\|php[3-5]?\)$"
  :hook (web-mode-hook . user--web-mode-hook)
  :init
  (when (feature-p 'polymode)
    (add-auto-mode 'poly-javascript-erb-mode "\\.js\\.erb$")
    (add-auto-mode 'poly-coffee-erb-mode "\\.coffee\\.erb$")
    (add-auto-mode 'poly-html-erb-mode "\\.html\\.erb$"))
  :config
  (validate-setq
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

  (use-package restclient
    :config
    (use-package company-restclient
      :after (company)
      :config
      (with-eval-after-load 'company
        (add-to-list 'company-backends 'company-restclient)))

    (use-package restclient-helm)

    (use-package ob-restclient
      :config
      (with-eval-after-load 'ob
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((restclient . t))))))

  (use-package tern
    :if (executable-find "npm")
    :hook (tern-mode-hook . user--tern-mode-hook)
    :config
    (use-package company-tern
      :after (company)))

  (use-package ac-html
    :after (auto-complete)
    :config
    (add-to-list 'web-mode-ac-sources-alist
                 '("html" . (ac-source-html-attribute-value
                             ac-source-html-tag
                             ac-source-html-attribute))))
  (use-package company-web
    :after (company))
  (use-package skewer-mode)
  (use-package tidy)

  (use-package cakecrumbs
    :init
    (cakecrumbs-auto-setup))

  (use-package lsp-html
    :ensure nil
    :if (executable-find "html-languageserver")
    :hook ((web-mode-hook . lsp-mode)
           (html-mode-hook . lsp-mode)))

  (use-package prettier
    :if (executable-find "prettier")
    :hook (after-init-hook . global-prettier-mode)))


(provide 'modes/web)
;;; web.el ends here
