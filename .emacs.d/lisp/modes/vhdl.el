;;; vhdl.el --- VHDL mode support
;;; Commentary:
;;; Code:

(defun user/vhdl-mode-hook ()
  "VHDL mode hook.")


(defun user/vhdl-mode-init ()
  "Initialize VHDL mode."
  (require-package '(:name vhdl-mode))

  (add-hook 'vhdl-mode-hook 'user/vhdl-mode-hook))

(user/vhdl-mode-init)


(provide 'modes/vhdl)
;;; vhdl.el ends here
