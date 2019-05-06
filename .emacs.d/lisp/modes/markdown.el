;;; markdown --- initializes Markdown modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-flymd-cache-directory*
  (path-join *user-cache-directory* "flymd")
  "Path to user's FLYMD cache store.")


(defun user--markdown-mode-hook ()
  "Markdown mode hook."
  (user/smartparens-enable)

  ;; org-mode table editing tools.
  (orgtbl-mode t))

(use-package markdown-mode
  :defer
  :hook (markdown-mode-hook . user--markdown-mode-hook)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (use-package flycheck-mmark
    :if (executable-find "mmark")
    :hook (flycheck-mode-hook . flycheck-mmark-setup))

  (use-package poly-markdown
    :after polymode
    :hook (markdown-mode-hook . poly-markdown-mode))

  (use-package markdown-preview-eww)
  (use-package markdown-preview-mode)

  (use-package mkdown
    :config
    (add-to-list 'markdown-css-paths mkdown-css-file-name))

  (use-package gh-md)

  (use-package flymd
    :init
    (make-directory *user-flymd-cache-directory* t)
    :config
    (validate-setq
     flymd-output-directory *user-flymd-cache-directory*))

  (use-package livedown
    :if (executable-find "livedown")
    :quelpa (livedown
             :fetcher github
             :repo "shime/emacs-livedown")
    :init
    (require 'livedown)))


(provide 'modes/markdown)
;;; markdown.el ends here
