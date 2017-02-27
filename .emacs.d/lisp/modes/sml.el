;;; sml.el --- Set up SML mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--sml-mode-hook ()
  "SML mode hook."
  (setq
   ;; Indent with spaces by default.
   indent-tabs-mode nil))

(with-executable 'sml
  (use-package sml-mode
    :defer
    :init
    (add-auto-mode 'sml-mode "\\.[Mm][Ll]$")
    (add-hook 'sml-mode-hook 'user--sml-mode-hook)))


(provide 'modes/sml)
;;; sml.el ends here
