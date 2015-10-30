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
  (require-package '(:name verilog-mode))
  (require-package '(:name auto-complete-verilog)))

(user/verilog-mode-init)


(provide 'modes/verilog)
;;; verilog.el ends here
