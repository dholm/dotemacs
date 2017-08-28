;;; tcl.el --- Tcl mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--tcl-mode-hook ()
  "Tcl mode hook."
  (user/gnu-global-enable))

(use-package tcl
  :defer
  :interpreter ("expect" . tcl-mode)
  :init
  (add-hook 'tcl-mode-hook 'user--tcl-mode-hook))


(provide 'modes/tcl)
;;; tcl.el ends here
