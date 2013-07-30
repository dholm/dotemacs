;;; markdown --- initializes Markdown modes
;;; Commentary:
;;; Code:

(defun dholm/markdown-mode-hook ()
  "Markdown mode hook.")


(defun dholm/markdown-mode-init ()
  "Initialize markdown mode."
  (add-hook 'markdown-mode-hook 'dholm/markdown-mode-hook))

(require-package '(:name markdown-mode :after (dholm/markdown-mode-init)))


(provide 'modes/markdown)
;;; markdown.el ends here
