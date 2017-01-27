;;; systemd.el --- Initializes systemd mode
;;; Commentary:
;;; Code:

(defun user/systemd-mode-hook ()
  "systemd mode hook.")


(defun user/systemd-init ()
  "Initialize systemd mode."
  ;;; (Hooks) ;;;
  (add-hook 'systemd-mode-hook 'user/systemd-mode-hook))

(use-package systemd
  :ensure t
  :config (user/systemd-init))


(provide 'modes/systemd)
;;; systemd.el ends here
