;;; go.el --- Go mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--go-mode-hook ()
  "Go mode hook."
  ;; Camel-case separates words.
  (subword-mode t)

  (when (feature-p 'go-eldoc)
    (go-eldoc-setup))

  (cond
   ((user/company-mode-p)
    (with-feature 'company-go
      (set
       (make-local-variable 'company-backends)
       '(company-go)))))

  ;;; (Bindings) ;;;
  (user/bind-key-local :doc :describe 'godef-describe)
  (user/bind-key-local :doc :reference 'godoc)
  (user/bind-key-local :nav :follow-symbol 'godef-jump)
  (user/bind-key-local :nav :switch-spec-impl 'go-goto-imports)
  (user/bind-key-local :debug :start 'realgud-gub)
  (when (feature-p 'gotest)
    ;; Prepend compilation error regexes from gotest for current
    ;; buffer.
    (dolist (elt (reverse go-test-compilation-error-regexp-alist))
      (add-to-list 'compilation-error-regexp-alist elt t))

    (user/bind-key-local :code :test 'go-test-current-project))
  (when (feature-p 'go-rename)
    (user/bind-key-local :code :refactor-rename 'go-rename))
  (user/bind-key-local :code :run 'go-run))

(use-package go-mode
  :if (executable-find "go")
  :defer
  :init
  (add-hook 'go-mode-hook 'user--go-mode-hook)
  :config
  (validate-setq
   ;; Hide errors as they are managed by Flycheck.
   gofmt-show-errors nil)

  (use-package go-autocomplete)
  (use-package company-go)
  (use-package go-eldoc)
  (use-package gotest
    :config
    ;; Copy compilation error regexes from gotest.
    (dolist (elt go-test-compilation-error-regexp-alist-alist)
      (add-to-list 'compilation-error-regexp-alist-alist elt)))
  (use-package go-rename
    :if (executable-find "gorename"))

  (use-package go-projectile)

  (when (feature-p 'helm)
    (use-package helm-go-package
      :bind (:map go-mode-map
                  ([remap go-import-add] . helm-go-package))))

  (use-package flycheck-gometalinter
    :if (executable-find "gometalinter")
    :config
    (validate-setq
     ;; Only check the project being worked on.
     flycheck-gometalinter-vendor nil)

    (flycheck-gometalinter-setup)))


(provide 'modes/go)
;;; go.el ends here
