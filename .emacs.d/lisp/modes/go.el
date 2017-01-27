;;; go.el --- Go mode support
;;; Commentary:
;;; Code:

(defun user--go-mode-hook ()
  "Go mode hook."
  ;; Automatic code fixes before saving
  (add-hook 'write-contents-functions
            '(lambda ()
               ;; Fix formatting
               (gofmt-before-save)))

  ;; Camel-case separates words.
  (subword-mode t)

  (when (feature-p 'go-eldoc)
    (go-eldoc-setup))

  (when (feature-p 'go-oracle)
    (go-oracle-mode t))

  (cond
   ((user/company-mode-p)
    (with-feature 'company-go
      (set
       (make-local-variable 'company-backends)
       '(company-go))))
   ((user/auto-complete-p)
    (user/ycmd-enable)))

  ;;; (Bindings) ;;;
  (user/bind-key-local :doc :describe 'godef-describe)
  (user/bind-key-local :doc :reference 'godoc)
  (user/bind-key-local :nav :follow-symbol 'godef-jump)
  (user/bind-key-local :nav :switch-spec-impl 'go-goto-imports)
  (user/bind-key-local :debug :start 'realgud-gub)
  (when (feature-p 'go-test)
    (user/bind-key-local :code :test 'go-test-current-file))
  (when (feature-p 'helm-go-package)
    (local-set-key [remap go-import-add] 'helm-go-package)))


(defun user--go-mode-config ()
  "Initialize Go mode."
  (add-hook 'go-mode-hook 'user--go-mode-hook))

(with-executable 'go
  (req-package go-mode
    :config (user--go-mode-config))
  (req-package go-autocomplete)
  (req-package company-go)
  (req-package go-eldoc)
  (req-package gotest)
  (req-package go-projectile)
  (req-package go-oracle
    :loader :el-get)
  (when (feature-p 'helm)
    (req-package helm-go-package)))


(provide 'modes/go)
;;; go.el ends here
