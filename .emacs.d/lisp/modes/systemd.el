;;; systemd.el --- Initializes systemd mode
;;; Commentary:
;;; Code:

(defun user--systemd-mode-hook ()
  "systemd mode hook.")


(defun user--systemd-config ()
  "Initialize systemd mode."
  ;;; (Hooks) ;;;
  (add-hook 'systemd-mode-hook 'user--systemd-mode-hook))

(use-package systemd
  :defer t
  :config (user--systemd-config))


(provide 'modes/systemd)
;;; systemd.el ends here
