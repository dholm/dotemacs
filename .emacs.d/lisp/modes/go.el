;;; go.el --- Go mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--go-mode-hook ()
  "Go mode hook."
  ;; Format Go code before saving it.
  (add-hook 'before-save-hook #'gofmt-before-save nil t)

  ;; Don't automatically break lines, gofmt will handle formatting.
  (auto-fill-mode -1)

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

  (user/tags-try-enable)

  ;; Disable whitespace mode settings that don't make sense in Go.
  (user/whitespace-disable-style '(lines lines-tail))

  (when (feature-p 'gotest)
    ;; Prepend compilation error regexes from gotest for current
    ;; buffer.
    (dolist (elt (reverse go-test-compilation-error-regexp-alist))
      (add-to-list 'compilation-error-regexp-alist elt t))))

(use-package go-mode
  :if (executable-find "go")
  :defer
  :bind-wrap
  (:map go-mode-map
        ((:key :doc :describe) . godef-describe)
        ((:key :doc :reference) . godoc)
        ((:key :nav :follow-symbol) . godef-jump)
        ((:key :nav :switch-spec-impl) . go-goto-imports))
  :hook (go-mode-hook . user--go-mode-hook)
  :config
  (validate-setq
   ;; Hide errors as they are managed by Flycheck.
   gofmt-show-errors nil)

  (use-package go-autocomplete)
  (use-package company-go)
  (use-package go-eldoc)
  (use-package gotest
    :bind-wrap
    (:map go-mode-map
          ((:key :code :test) . go-test-current-project)
          ((:key :code :run) . go-run))
    :config
    ;; Copy compilation error regexes from gotest.
    (dolist (elt go-test-compilation-error-regexp-alist-alist)
      (add-to-list 'compilation-error-regexp-alist-alist elt)))
  (use-package godoctor
    :if (executable-find "godoctor")
    :bind-wrap
    (:map go-mode-map
          ((:key :code :refactor-rename) . godoctor-rename)
          ((:key :code :refactor-extract) . godoctor-extract)
          ((:key :code :document) . godoctor-godoc)))
  (use-package go-tag
    :if (executable-find "gomodifytags"))

  (use-package go-projectile)

  (when (feature-p 'helm)
    (use-package helm-go-package
      :bind (:map go-mode-map
                  ([remap go-import-add] . helm-go-package))))

  (use-package flycheck-gometalinter
    :if (executable-find "gometalinter")
    :config
    (validate-setq
     ;; Default to using fast linters only.
     flycheck-gometalinter-fast t
     ;; Only check the project being worked on.
     flycheck-gometalinter-vendor nil)

    (flycheck-gometalinter-setup))

  (use-package go-fill-struct
    :if (executable-find "fillstruct"))

  (use-package go-imports
    :if (executable-find "goimports")
    :config
    (validate-setq
     ;; Use goimports as formatter so that imports are updated at the
     ;; same time.
     gofmt-command "goimports"))

  (use-package lsp-go
    :if (executable-find "go-langserver")
    :hook (go-mode-hook . go-lsp-enable))

  (use-package rats
    :hook (go-mode-hook . rats-mode)
    :bind-wrap (:map rats-mode-map
                     ((:key :code :test)
                      . rats-run-test-in-current-buffer))
    :config
    (with-eval-after-load 'popwin
      (add-to-list
       'popwin:special-display-config
       ;; Don't select compilation window when shown
       '(go-test-mode :height 20 :dedicated t))))

  (use-package gorepl-mode
    :if (executable-find "gore")
    :diminish gorepl-mode
    :hook (go-mode-hook . gorepl-mode)
    :bind-wrap (:map gorepl-mode-map
                     ((:key :code :eval-expression) . gorepl-eval-line)
                     ((:key :code :eval-selection)
                      . gorepl-eval-region)))

  (use-package go-gen-test
    :if (executable-find "gotests")
    :bind-wrap (:map go-mode-map
                     ((:key :code :generate-test) . go-gen-test-dwim))))


(provide 'modes/go)
;;; go.el ends here
