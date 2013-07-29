;;; markdown --- initializes Markdown modes
;;; Commentary:
;;; Code:

(defun dholm/markdown-mode-hook ()
  "Markdown mode hook."
  (setq-default
   ;; Indent using spaces
   indent-tabs-mode nil)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t))


(defun dholm/markdown-mode-init ()
  "Initialize markdown mode."
  (add-hook 'markdown-mode-hook 'dholm/markdown-mode-hook))

(require-package '(:name markdown-mode :after (dholm/markdown-mode-init)))


(provide 'modes/markdown)
;;; markdown.el ends here
