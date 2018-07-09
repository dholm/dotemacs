;;; lsp.el --- Language server protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook (lsp-after-open-hook . lsp-enable-imenu)
  :config
  (use-package lsp-ui
    :hook (lsp-mode-hook . lsp-ui-mode))
  (use-package company-lsp
    :after (company)
    :config
    (validate-setq
     company-lsp-enable-snippet t
     ;; Enable automatic chained completions.
     company-lsp-enable-recompletion t
     ;; Let the server handle caching.
     company-lsp-async t
     company-lsp-cache-candidates nil)

    (add-to-list 'company-backends 'company-lsp)))


(provide 'utilities/lsp)
;;; lsp.el ends here
