;;; markdown --- initializes Markdown modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--markdown-mode-hook ()
  "Markdown mode hook."
  (user/smartparens-enable)

  (when (feature-p 'polymode)
    (poly-markdown-mode t)))

(use-package markdown-mode
  :defer
  :init
  (add-hook 'markdown-mode-hook 'user--markdown-mode-hook)
  :config
  (with-eval-after-load 'smartparens
    (defun sp--gfm-skip-asterisk (ms mb me)
      (save-excursion
        (goto-char mb)
        (save-match-data (looking-at "^\\* "))))

    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'sp--gfm-skip-asterisk)
      (sp-local-pair "`" "`")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags)))

  (use-package livedown
    :if (executable-find "npm")
    :quelpa (livedown
             :fetcher github
             :repo "shime/emacs-livedown")
    :init
    (require 'livedown)))


(provide 'modes/markdown)
;;; markdown.el ends here
