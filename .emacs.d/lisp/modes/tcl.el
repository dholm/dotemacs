;;; tcl.el --- Tcl mode support
;;; Commentary:
;;; Code:

(defun user--tcl-mode-hook ()
  "Tcl mode hook."
  (user/gnu-global-enable))

(use-package tcl
  :defer
  :init
  ;; Use tcl-mode for expect scripts
  (add-interpreter-mode 'tcl-mode "expect")

  (add-hook 'tcl-mode-hook 'user--tcl-mode-hook))


(provide 'modes/tcl)
;;; tcl.el ends here
