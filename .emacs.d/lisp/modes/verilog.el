;;; verilog.el --- Verilog mode support
;;; Commentary:
;;; Code:

(defun user--verilog-mode-hook ()
  "Verilog mode hook."
  (user/gnu-global-enable))


(defun user--verilog-mode-config ()
  "Initialize Verilog mode."
  (when (feature-p 'polymode)
    (add-auto-mode 'poly-verilog+perl-mode "\\.sv$" "\\.svh$"))

  ;;; (Hooks) ;;;
  (add-hook 'verilog-mode-hook 'user--verilog-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name verilog-mode))
  (use-package auto-complete-verilog
    :defer t
    :requires auto-complete
    :quelpa (auto-complete-verilog
             :fetcher wiki
             :files ("auto-complete-verilog.el"))))

(user--verilog-mode-config)


(provide 'modes/verilog)
;;; verilog.el ends here
