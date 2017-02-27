;;; vhdl.el --- VHDL mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--vhdl-mode-hook ()
  "VHDL mode hook."
  (user/gnu-global-enable))

(use-package vhdl-mode
  :defer
  :quelpa (vhdl-mode
           :fetcher url
           :url "https://guest.iis.ee.ethz.ch/~zimmi/emacs/vhdl-mode-3.38.1.tar.gz")
  :init
  (add-auto-mode 'vhdl-mode "\\.vhdl?$")
  (add-hook 'vhdl-mode-hook 'user--vhdl-mode-hook))


(provide 'modes/vhdl)
;;; vhdl.el ends here
