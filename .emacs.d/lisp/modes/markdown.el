;;; markdown --- initializes Markdown modes
;;; Commentary:
;;; Code:

(defun user--markdown-mode-hook ()
  "Markdown mode hook."
  (user/smartparens-enable)

  (when (feature-p 'polymode)
    (poly-markdown-mode t))

  (with-feature 'guide-key
    (guide-key/add-local-highlight-command-regexp "markdown-")))


(defun user--markdown-mode-config ()
  "Initialize markdown mode."
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

  ;;; (Hooks) ;;;
  (add-hook 'markdown-mode-hook 'user--markdown-mode-hook))

(use-package markdown-mode
  :ensure t
  :config (user--markdown-mode-config))
(with-executable 'npm
  (require-package '(:name livedown)))


(provide 'modes/markdown)
;;; markdown.el ends here
