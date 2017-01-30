;;; debugger.el --- Emacs debugger mode
;;; Commentary:
;;; Code:

(defun user--debugger-mode-hook ()
  "Debugger mode hook."
  (hl-line-mode t))


(defun user--debugger-config ()
  "Initialize debugger mode."
  (add-hook 'debugger-mode-hook 'user--debugger-mode-hook))

(after-load 'debug
  (user--debugger-config))


(provide 'modes/debugger)
;;; debugger.el ends here
