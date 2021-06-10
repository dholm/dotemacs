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
   lsp-enable-file-watchers nil
   ;; Increase the amount of data that can be read from a language server.
   read-process-output-max (* 3 1024 1024))

  (use-package lsp-ui
    :hook (lsp-mode-hook . lsp-ui-mode)
    :config
    (validate-setq
     ;; Disable the sideline popup.
     lsp-ui-sideline-enable nil)))


(provide 'utilities/lsp)
;;; lsp.el ends here
