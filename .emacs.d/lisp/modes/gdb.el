;;; gdb.el --- initializes GDB modes
;;; Commentary:
;;; Code:

(defun user/gud-mode-hook ()
  "GDB gud mode hook."
  ;;; (Bindings) ;;;
  ;; Breakpoints
  (define-key user/code-map (kbd "b") 'gud-break)
  (define-key user/code-map (kbd "t") 'gud-tbreak)
  (define-key user/code-map (kbd "w") 'gud-watch)
  (define-key user/code-map (kbd "d") 'gud-remove)

  ;; Stepping
  (define-key user/code-map (kbd "s") 'gud-step)
  (define-key user/code-map (kbd "i") 'gud-stepi)
  (define-key user/code-map (kbd "n") 'gud-next)

  ;; Execution
  (define-key user/code-map (kbd "r") 'gud-run)
  (define-key user/code-map (kbd "c") 'gud-cont)
  (define-key user/code-map (kbd "f") 'gud-finish)
  (define-key user/code-map (kbd "u") 'gud-until)

  ;; Stack frames
  (define-key user/navigation-map (kbd "p") 'gud-up)
  (define-key user/navigation-map (kbd "n") 'gud-down)

  ;; Printing
  (define-key user/documentation-map (kbd "p") 'gud-print))


(defun user/gdb-mode-hook ()
  "GDB mode hook."
  ;; Navigation
  (define-key user/navigation-map (kbd "t") 'gdb-frame-threads-buffer)
  (define-key user/navigation-map (kbd "b") 'gdb-frame-breakpoints-buffer)
  (define-key user/navigation-map (kbd "m") 'gdb-frame-memory-buffer)
  (define-key user/navigation-map (kbd "d") 'gdb-frame-disassembly-buffer)
  (define-key user/navigation-map (kbd "r") 'gdb-frame-registers-buffer)
  (define-key user/navigation-map (kbd "i") 'gdb-frame-io-buffer)
  (define-key user/navigation-map (kbd "l") 'gdb-frame-locals-buffer)
  (define-key user/navigation-map (kbd "s") 'gdb-frame-stack-buffer))


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
