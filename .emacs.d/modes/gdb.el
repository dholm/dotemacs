;;; gdb --- initializes GDB modes
;;; Commentary:
;;; Code:

(defun dholm/gdb-script-mode-hook ()
  "GDB script mode hook."
  ;; When using gud-mode to debug enable gdb-many-windows and a separate IO buffer
  (setq
   gdb-many-windows t
   gdb-use-separate-io-buffer t
   gdb-show-main t
   gud-chdir-before-run nil
   gud-tooltip-mode t))


(defun dholm/gdb-script-mode-init ()
  "Initialize GDB script mode."
  (add-auto-mode 'gdb-script-mode "\\.gdb$" "\\.gdbinit$"))

(dholm/gdb-script-mode-init)


(provide 'modes/gdb)
;;; gdb.el ends here
