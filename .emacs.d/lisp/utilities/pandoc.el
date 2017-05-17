;;; pandoc.el --- Interface to Pandoc -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--pandoc-mode-hook ()
  "Pandoc mode hook."
  (pandoc-load-default-settings))


(use-package pandoc-mode
  :if (executable-find "pandoc")
  :defer
  :diminish pandoc-mode
  :init
  (add-hook 'pandoc-mode 'user--pandoc-mode-hook))


(provide 'utilities/pandoc)
;;; pandoc.el ends here
