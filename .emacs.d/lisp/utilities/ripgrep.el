;;; ripgrep.el --- interface to ripgrep -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rg
  :if (executable-find "rg")
  :config
  (use-package helm-rg))


(provide 'utilities/ripgrep)
;;; ripgrep.el ends here
