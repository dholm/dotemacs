;;; verilog.el --- Verilog mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--verilog-mode-hook ()
  "Verilog mode hook."
  (user/gnu-global-enable))


(use-package verilog-mode
  :defer
  :mode "\\.[ds]?vh?$"
  :hook (verilog-mode-hook . user--verilog-mode-hook)
  :config
  (when (feature-p 'polymode)
    (add-auto-mode 'poly-verilog+perl-mode "\\.sv$" "\\.svh$"))

  (use-package auto-complete-verilog
    :after (auto-complete)
    :quelpa (auto-complete-verilog
             :fetcher url
             :url
             "http://www.emacswiki.org/emacs-en/download/auto-complete-verilog.el"))

  (use-package veri-kompass
    :hook (verilog-mode-hook . veri-kompass-minor-mode)
    :bind-wrap
    (:map verilog-mode-map
     ((:key :nav :functions/toc) . veri-kompass))))


(provide 'modes/verilog)
;;; verilog.el ends here
