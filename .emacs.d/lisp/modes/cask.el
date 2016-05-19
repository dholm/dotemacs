;;; cask.el --- Cask mode support
;;; Commentary:
;;; Code:

(defun user/cask-mode-hook ()
  "Cask mode hook."
  (with-feature 'rainbow-delimiters
    (rainbow-delimiters-mode t))

  (with-feature 'paredit
    (enable-paredit-mode)
    (after-load 'diminish
      (diminish 'paredit-mode))))


(defun user/cask-mode-init ()
  "Initialize Cask mode."
  ;;; (Hooks) ;;;
  (add-hook 'cask-mode-hook 'user/cask-mode-hook))

(require-package '(:name cask-mode :after (user/cask-mode-init)))


(provide 'modes/cask)
;;; cask.el ends here
