;;; lsp.el --- Language server protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook (lsp-after-open-hook . lsp-enable-imenu)
  :config
  (use-package lsp-ui
    :hook (lsp-mode-hook . lsp-ui-mode))
  (use-package company-lsp
    :after (company-mode)
    :config
    (validate-setq
     company-lsp-enable-snippet t
     company-lsp-cache-candidates t)
    (add-to-list 'company-backends 'company-lsp)))


(provide 'utilities/lsp)
;;; lsp.el ends here
