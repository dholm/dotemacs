;;; verilog.el --- Verilog mode support
;;; Commentary:
;;; Code:

(defun user--verilog-mode-hook ()
  "Verilog mode hook."
  (user/gnu-global-enable))


(use-package verilog-mode
  :defer t
  :quelpa (verilog-mode
           :fetcher url
           :url "http://www.veripool.org/ftp/verilog-mode.el")
  :init
  (add-auto-mode 'verilog-mode "\\.[ds]?vh?$")
  :config
  (when (feature-p 'polymode)
    (add-auto-mode 'poly-verilog+perl-mode "\\.sv$" "\\.svh$"))

  ;;; (Hooks) ;;;
  (add-hook 'verilog-mode-hook 'user--verilog-mode-hook)

  ;;; (Packages) ;;;
  (use-package auto-complete-verilog
    :defer t
    :requires auto-complete
    :quelpa (auto-complete-verilog
             :fetcher wiki
             :files ("auto-complete-verilog.el"))))


(provide 'modes/verilog)
;;; verilog.el ends here
