;;; markdown --- initializes Markdown modes
;;; Commentary:
;;; Code:

(defun user/markdown-mode-hook ()
  "Markdown mode hook.")


(defun user/markdown-mode-init ()
  "Initialize markdown mode."
  (add-hook 'markdown-mode-hook 'user/markdown-mode-hook))

(require-package '(:name markdown-mode :after (user/markdown-mode-init)))
(with-executable 'npm
  (require-package '(:name livedown)))


(provide 'modes/markdown)
;;; markdown.el ends here
