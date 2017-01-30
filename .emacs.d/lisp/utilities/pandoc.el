;;; pandoc.el --- Interface to Pandoc
;;; Commentary:
;;; Code:

(defun user--pandoc-mode-hook ()
  "Pandoc mode hook."
  (pandoc-load-default-settings))


(defun user--pandoc-mode-config ()
  "Initialize Pandoc mode."
  (add-hook 'pandoc-mode 'user--pandoc-mode-hook))

(with-executable 'pandoc
  (use-package pandoc-mode
    :defer t
    :config (user--pandoc-mode-config)))


(provide 'utilities/pandoc)
;;; pandoc.el ends here
