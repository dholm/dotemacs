;;; asciidoc.el --- AsciiDoc mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--adoc-mode-hook ()
  "AsciiDoc mode hook."
  (user--text-mode-hook))

(use-package adoc-mode
  :defer
  :mode "\.\(adoc\|asciidoc\)$"
  :init
  (add-hook 'adoc-mode-hook 'user--adoc-mode-hook))


(provide 'modes/asciidoc)
;;; asciidoc.el ends here
