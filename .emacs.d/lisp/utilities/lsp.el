;;; lsp.el --- Language server protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--lsp-mode-hook ()
  "Mode hook for LSP minor modes."
  (user/tags-try-enable))

(use-package lsp-mode
  :pin "MELPA"
  :commands lsp
  :hook ((lsp-after-open-hook . lsp-enable-imenu)
         (lsp-mode-hook . user--lsp-mode-hook))
  :config
  (validate-setq
   ;; Prefer Flycheck.
   lsp-prefer-flymake nil
   ;; Location of persistent LSP session.
   lsp-session-file (path-join *user-cache-directory* "lsp-session"))

  (use-package lsp-ui
    :pin "MELPA"
    :hook (lsp-mode-hook . lsp-ui-mode))
  (use-package company-lsp
    :pin "MELPA"
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
