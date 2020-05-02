;;; java.el --- Java mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--java-mode-hook ()
  "Java mode hook."
  (with-feature 'lsp
    (lsp)))

(use-package cc-mode
  :ensure nil
  :if (executable-find "javac")
  :defer
  :hook (java-mode-hook . user--java-mode-hook)
  :config
  (use-package lsp-java
    :pin "MELPA"
    :config
    (validate-setq
     ;; Path to LSP server.
     lsp-java-server-install-dir (path-join *user-cache-directory* "lsp-java")))

  (use-package mvn))


(provide 'modes/java)
;;; java.el ends here
