;;; lsp.el --- Language server protocol. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--lsp-mode-hook ()
  "Mode hook for LSP minor modes.")

(use-package lsp-mode
  :pin "MELPA"
  :commands (lsp lsp-deferred)
  :hook ((lsp-after-open-hook . lsp-enable-imenu)
         (lsp-mode-hook . user--lsp-mode-hook))
  :config
  (validate-setq
   ;; Automatically try to figure out project root.
   lsp-auto-guess-root t
   ;; Location of persistent LSP session.
   lsp-session-file (path-join *user-cache-directory* "lsp-session")
   ;; Disable yasnippet integration.
   lsp-enable-snippet nil
   ;; Don't watch files for changes, prevents freeze during compilation.
   lsp-enable-file-watchers nil)

  (use-package lsp-ui
    :hook (lsp-mode-hook . lsp-ui-mode)
    :config
    (validate-setq
     ;; Disable the sideline popup.
     lsp-ui-sideline-enable nil))

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
