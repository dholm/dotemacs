;;; sml.el --- Set up SML mode
;;; Commentary:
;;; Code:

(defun user/sml-mode-hook ()
  "SML mode hook.")


(defun user/sml-mode-init ()
  "Initialize SML mode."
  (add-auto-mode 'sml-mode "\\.[Mm][Ll]$")

  ;;; (Hooks) ;;;
  (add-hook 'sml-mode-hook 'user/sml-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name sml-mode :after (user/sml-mode-init))))

(with-executable 'sml
  (user/sml-mode-init))


(provide 'modes/sml)
;;; sml.el ends here
