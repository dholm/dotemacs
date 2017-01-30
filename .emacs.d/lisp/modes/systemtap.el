;;; systemtap.el --- SystemTap mode support
;;; Commentary:
;;; Code:

(defun user--systemtap-mode-hook ()
  "SystemTap mode hook.")


(defun user--systemtap-mode-config ()
  "Initialize SystemTap mode."
  ;;; (Hooks) ;;;
  (add-hook 'systemtap-mode-hook 'user--systemtap-mode-hook))

(with-executable 'stap
  (use-package systemtap-mode
    :ensure t
    :config (user--systemtap-mode-config)))


(provide 'modes/systemtap)
;;; systemtap.el ends here
