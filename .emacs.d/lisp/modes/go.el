;;; go.el --- Go mode support -*- lexical-binding: t; -*-
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
  (when (feature-p 'go-test)
    (user/bind-key-local :code :test 'go-test-current-file)))

(with-executable 'go
  (use-package go-mode
    :defer
    :init
    (add-hook 'go-mode-hook 'user--go-mode-hook)
    :config
    (use-package go-autocomplete)
    (use-package company-go)
    (use-package go-eldoc)
    (use-package gotest)
    (use-package go-projectile)

    (when (feature-p 'helm)
      (use-package helm-go-package
        :bind (:map go-mode-map
                    ([remap go-import-add] . helm-go-package))))))


(provide 'modes/go)
;;; go.el ends here
