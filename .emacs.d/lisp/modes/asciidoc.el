;;; asciidoc.el --- AsciiDoc mode support
;;; Commentary:
;;; Code:

(defun user/adoc-mode-hook ()
  "AsciiDoc mode hook."
  (user/text-mode-hook))


(defun user/adoc-mode-init ()
  "Initialize AsciiDoc mode."
  (add-hook 'adoc-mode-hook 'user/adoc-mode-hook)

  (add-auto-mode 'adoc-mode "\\.adoc$" "\\.asciidoc$"))

(require-package '(:name adoc-mode :after (user/adoc-mode-init)))


(provide 'modes/asciidoc)
;;; asciidoc.el ends here
