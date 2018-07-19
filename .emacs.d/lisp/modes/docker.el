;;; docker.el --- Configure Emacs Docker mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dockerfile-mode
  :config
  (use-package lsp-dockerfile
    :if (executable-find "docker-langserver")
    :quelpa (lsp-dockerfile :fetcher github :repo "emacs-lsp/lsp-dockerfile")
    :hook (dockerfile-mode-hook . lsp-dockerfile-enable)))

(use-package docker-compose-mode)


(provide 'modes/docker)
;;; docker.el ends here
