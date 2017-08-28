;;; sml.el --- Set up SML mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--sml-mode-hook ()
  "SML mode hook."
  (setq
   ;; Indent with spaces by default.
   indent-tabs-mode nil))

(use-package sml-mode
  :if (executable-find "sml")
  :defer
  :mode "\\.[Mm][Ll]$"
  :init
  (add-hook 'sml-mode-hook 'user--sml-mode-hook))


(provide 'modes/sml)
;;; sml.el ends here
