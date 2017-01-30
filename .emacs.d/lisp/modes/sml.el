;;; sml.el --- Set up SML mode
;;; Commentary:
;;; Code:

(defun user--sml-mode-hook ()
  "SML mode hook."
  (setq
   ;; Indent with spaces by default.
   indent-tabs-mode nil))


(defun user--sml-mode-config ()
  "Initialize SML mode."
  (add-auto-mode 'sml-mode "\\.[Mm][Ll]$")

  ;;; (Hooks) ;;;
  (add-hook 'sml-mode-hook 'user--sml-mode-hook)

  ;;; (Packages) ;;;
  (use-package sml-mode
    :ensure t
    :config (user--sml-mode-config)))

(with-executable 'sml
  (user--sml-mode-config))


(provide 'modes/sml)
;;; sml.el ends here
