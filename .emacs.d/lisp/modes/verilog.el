;;; verilog.el --- Verilog mode support
;;; Commentary:
;;; Code:

(defun user/verilog-mode-hook ()
  "Verilog mode hook."
  (user/gnu-global-enable))


(defun user/verilog-mode-init ()
  "Initialize Verilog mode."
  (when (feature-p 'polymode)
    (add-auto-mode 'poly-verilog+perl-mode "\\.sv$" "\\.svh$"))

  ;;; (Hooks) ;;;
  (add-hook 'verilog-mode-hook 'user/verilog-mode-hook)

  ;;; (Packages) ;;;
  (req-package verilog-mode
    :loader :el-get)
  (req-package auto-complete-verilog
    :loader :el-get))

(user/verilog-mode-init)


(provide 'modes/verilog)
;;; verilog.el ends here
