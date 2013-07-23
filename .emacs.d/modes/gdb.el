;;; gdb --- initializes GDB modes
;;; Commentary:
;;; Code:

;; When using gud-mode to debug enable gdb-many-windows and a separate IO buffer
(setq gdb-many-windows t
      gdb-use-separate-io-buffer t
      gdb-show-main t
      gud-chdir-before-run nil
      gud-tooltip-mode t)

;; Use gdb-script-mode for files ending in .gdb
(setq auto-mode-alist
      (cons '("\\.gdb$" . gdb-script-mode) auto-mode-alist))


(provide 'modes/gdb)
;;; gdb.el ends here
