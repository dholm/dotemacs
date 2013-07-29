;;; markdown --- initializes Markdown modes
;;; Commentary:
;;; Code:

(defun dholm/markdown-mode-init ()
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode)))

(require-package '(:name markdown-mode :after (dholm/markdown-mode-init)))


(provide 'modes/markdown)
;;; markdown.el ends here
