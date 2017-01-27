;;; vhdl.el --- VHDL mode support
;;; Commentary:
;;; Code:

(defun user--vhdl-mode-hook ()
  "VHDL mode hook."
  (user/gnu-global-enable))


(defun user--vhdl-mode-config ()
  "Initialize VHDL mode."
  (req-package vhdl-mode
    :loader :el-get)

  (add-hook 'vhdl-mode-hook 'user--vhdl-mode-hook))

(user--vhdl-mode-config)


(provide 'modes/vhdl)
;;; vhdl.el ends here
