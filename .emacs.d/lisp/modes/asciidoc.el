;;; asciidoc.el --- AsciiDoc mode support
;;; Commentary:
;;; Code:

(defun user--adoc-mode-hook ()
  "AsciiDoc mode hook."
  (user--text-mode-hook))


(defun user--adoc-mode-config ()
  "Initialize AsciiDoc mode."
  (add-hook 'adoc-mode-hook 'user--adoc-mode-hook)

  (add-auto-mode 'adoc-mode "\\.adoc$" "\\.asciidoc$"))

(use-package adoc-mode
  :ensure t
  :config (user--adoc-mode-config))


(provide 'modes/asciidoc)
;;; asciidoc.el ends here
