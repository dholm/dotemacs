;;; tcl.el --- Tcl mode support
;;; Commentary:
;;; Code:

(defun user--tcl-mode-hook ()
  "Tcl mode hook."
  (user/gnu-global-enable))

(defun user--tcl-mode-config ()
  "Initialize Tcl mode."
  ;; Use tcl-mode for expect scripts
  (add-interpreter-mode 'tcl-mode "expect")

  (add-hook 'tcl-mode-hook 'user--tcl-mode-hook))

(user--tcl-mode-config)


(provide 'modes/tcl)
;;; tcl.el ends here
