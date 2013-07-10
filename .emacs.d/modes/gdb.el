;; When using gud-mode to debug enable gdb-many-windows and a separate IO buffer
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gud-tooltip-mode t)


;; Use gdb-script-mode for files ending in .gdb
(setq auto-mode-alist
      (cons '("\\.gdb$" . gdb-script-mode) auto-mode-alist))


(provide 'modes/gdb)
