;;; asciidoc.el --- AsciiDoc mode support
;;; Commentary:
;;; Code:

(defun user--adoc-mode-hook ()
  "AsciiDoc mode hook."
  (user--text-mode-hook))

(use-package adoc-mode
  :defer t
  :init
  (add-hook 'adoc-mode-hook 'user--adoc-mode-hook)

  (add-auto-mode 'adoc-mode "\\.adoc$" "\\.asciidoc$"))


(provide 'modes/asciidoc)
;;; asciidoc.el ends here
