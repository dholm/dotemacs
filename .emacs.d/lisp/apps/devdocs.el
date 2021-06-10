;;; devdocs.el --- sets up the Emacs Devdocs Browser -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package devdocs-browser
  :config
  (validate-setq
   ;; Cache location.
   devdocs-browser-cache-directory (path-join *user-cache-directory* "devdocs-browser")))


(provide 'apps/devdocs)
;;; devdocs.el ends here
