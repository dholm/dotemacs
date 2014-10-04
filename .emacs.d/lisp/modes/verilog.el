;;; verilog.el --- Verilog mode support
;;; Commentary:
;;; Code:

(defun user/verilog-mode-hook ()
  "Verilog mode hook."
  (user/gnu-global-enable))


(defun user/verilog-mode-init ()
  "Initialize Verilog mode."
  (require-package '(:name verilog-mode))
  (require-package '(:name auto-complete-verilog))

  (add-hook 'verilog-mode-hook 'user/verilog-mode-hook))

(user/verilog-mode-init)


(provide 'modes/verilog)
;;; verilog.el ends here
