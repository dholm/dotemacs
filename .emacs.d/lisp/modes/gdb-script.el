;;; gdb-script.el --- Initializes GDB script mode
;;; Commentary:
;;; Code:

(defun user--gdb-script-mode-hook ()
  "GDB script mode hook.")


(defun user--gdb-script-mode-config ()
  "Initialize GDB script mode."
  ;;; (Hooks) ;;;
  (add-auto-mode 'gdb-script-mode "\\.gdb$" "\\.gdbinit$")
  (add-hook 'gdb-script-mode-hook 'user--gdb-script-mode-hook))

(with-executable 'gdb
  (user--gdb-script-mode-config))


(provide 'modes/gdb-script)
;;; gdb-script.el ends here
