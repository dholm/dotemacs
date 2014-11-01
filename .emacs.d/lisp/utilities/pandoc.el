;;; pandoc.el --- Interface to Pandoc
;;; Commentary:
;;; Code:

(defun user/pandoc-mode-hook ()
  "Pandoc mode hook."
  (pandoc-load-default-settings))


(defun user/pandoc-mode-init ()
  "Initialize Pandoc mode."
  (add-hook 'pandoc-mode 'user/pandoc-mode-hook))

(with-executable 'pandoc
  (require-package '(:name pandoc-mode :after (user/pandoc-mode-init))))


(provide 'utilities/pandoc)
;;; pandoc.el ends here
