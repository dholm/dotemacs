;;; cask.el --- Cask mode support
;;; Commentary:
;;; Code:

(defun user--cask-mode-hook ()
  "Cask mode hook."
  (with-feature 'rainbow-delimiters
    (rainbow-delimiters-mode t))

  (with-feature 'paredit
    (enable-paredit-mode)))

(use-package cask-mode
  :defer t
  :init
  (add-hook 'cask-mode-hook 'user--cask-mode-hook))


(provide 'modes/cask)
;;; cask.el ends here
