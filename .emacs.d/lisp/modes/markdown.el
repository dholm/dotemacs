;;; markdown --- initializes Markdown modes
;;; Commentary:
;;; Code:

(defun user--markdown-mode-hook ()
  "Markdown mode hook."
  (user/smartparens-enable)

  (when (feature-p 'polymode)
    (poly-markdown-mode t)))

(use-package markdown-mode
  :defer t
  :init
  (add-hook 'markdown-mode-hook 'user--markdown-mode-hook)
  :config
  (after-load 'smartparens
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

  (with-executable 'npm
    (use-package livedown
      :quelpa (livedown
               :fetcher github
               :repo "shime/emacs-livedown"))
    (require 'livedown)))


(provide 'modes/markdown)
;;; markdown.el ends here
