;;; vhdl.el --- VHDL mode support
;;; Commentary:
;;; Code:

(defun user/vhdl-mode-hook ()
  "VHDL mode hook."
  (user/gnu-global-enable))


(defun user/vhdl-mode-init ()
  "Initialize VHDL mode."
  (req-package vhdl-mode
    :loader :el-get)

  (add-hook 'vhdl-mode-hook 'user/vhdl-mode-hook))

(user/vhdl-mode-init)


(provide 'modes/vhdl)
;;; vhdl.el ends here
