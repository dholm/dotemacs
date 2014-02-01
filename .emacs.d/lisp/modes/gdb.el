;;; gdb.el --- initializes GDB modes
;;; Commentary:
;;; Code:

(defun user/gud-mode-hook ()
  "GDB gud mode hook."
  ;;; (Bindings) ;;;
  ;; Breakpoints
  (user/bind-key-local :debug :break 'gud-break)
  (user/bind-key-local :debug :break-temporary 'gud-tbreak)
  (user/bind-key-local :debug :watch 'gud-watch)

  ;; Stepping
  (user/bind-key-local :debug :step 'gud-step)
  (user/bind-key-local :debug :step-instruction 'gud-stepi)
  (user/bind-key-local :debug :next 'gud-next)

  ;; Execution
  (user/bind-key-local :debug :run 'gud-run)
  (user/bind-key-local :debug :continue 'gud-cont)
  (user/bind-key-local :debug :continue-stack 'gud-finish)
  (user/bind-key-local :debug :continue-until 'gud-until)

  ;; Stack frames
  (user/bind-key-local :debug :stack-up 'gud-up)
  (user/bind-key-local :debug :stack-down 'gud-down)

  ;; Printing
  (user/bind-key-local :debug :show-value 'gud-print))


(defun user/gdb-mode-hook ()
  "GDB mode hook.")


(defun user/gdb-script-mode-hook ()
  "GDB script mode hook.")


(defun user/gdb-modes-init ()
  "Initialize GDB modes."
  ;; Configure gdb-mode
  (setq-default
   ;; Enable many windows
   gdb-many-windows t
   ;; Use a separate buffer for IO
   gdb-use-separate-io-buffer t
   gdb-show-main t)

  ;; Configure gud-mode
  (setq-default
   ;; Do not change the working directory before running inferior
   gud-chdir-before-run nil
   ;; Enable tooltips in gud mode
   gud-tooltip-mode t)

  (add-auto-mode 'gdb-script-mode "\\.gdb$" "\\.gdbinit$")

  (add-hook 'gud-mode-hook 'user/gud-mode-hook)
  (add-hook 'gdb-mode-hook 'user/gdb-mode-hook)
  (add-hook 'gdb-script-mode-hook 'user/gdb-script-mode-hook))

(when *has-gdb*
  (user/gdb-modes-init))


(provide 'modes/gdb)
;;; gdb.el ends here
