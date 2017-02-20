;;; sml.el --- Set up SML mode
;;; Commentary:
;;; Code:

(defun user--sml-mode-hook ()
  "SML mode hook."
  (setq
   ;; Indent with spaces by default.
   indent-tabs-mode nil))

(with-executable 'sml
  (use-package sml-mode
    :defer t
    :init
    (add-auto-mode 'sml-mode "\\.[Mm][Ll]$")
    (add-hook 'sml-mode-hook 'user--sml-mode-hook)))


(provide 'modes/sml)
;;; sml.el ends here
