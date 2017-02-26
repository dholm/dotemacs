;;; debugger.el --- Emacs debugger mode
;;; Commentary:
;;; Code:

(defun user--debugger-mode-hook ()
  "Debugger mode hook."
  (hl-line-mode t))

(use-package debug
  :defer
  :init
  (add-hook 'debugger-mode-hook 'user--debugger-mode-hook))


(provide 'modes/debugger)
;;; debugger.el ends here
