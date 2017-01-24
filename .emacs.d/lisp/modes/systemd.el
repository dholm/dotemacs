;;; systemd.el --- Initializes systemd mode
;;; Commentary:
;;; Code:

(defun user/systemd-mode-hook ()
  "systemd mode hook.")


(defun user/systemd-mode-init ()
  "Initialize systemd mode."
  ;;; (Hooks) ;;;
  (add-hook 'systemd-mode-hook 'user/systemd-mode-hook))

(require-package '(:name systemd-mode :after (user/systemd-mode-init)))


(provide 'modes/systemd)
;;; systemd.el ends here
