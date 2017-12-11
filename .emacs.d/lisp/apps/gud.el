;;; gud.el --- Initializes GUD debugger -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--gud-mode-hook ()
  "GDB gud mode hook."
  (gud-tooltip-mode t)

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

(defun user/realgud-mode-hook ()
  "RealGud mode hook."
  ;;; (Bindings) ;;;
  ;; Breakpoints
  (user/bind-key-local :debug :break 'realgud:cmd-break)
  (user/bind-key-local :debug :watch 'realgud:cmd-watch)

  ;; Stepping
  (user/bind-key-local :debug :step 'realgud:cmd-step)
  (user/bind-key-local :debug :step-instruction 'realgud:cmd-stepi)
  (user/bind-key-local :debug :next 'realgud:cmd-next)

  ;; Execution
  (user/bind-key-local :debug :run 'realgud:cmd-restart)
  (user/bind-key-local :debug :continue 'realgud:cmd-continue)
  (user/bind-key-local :debug :continue-stack 'realgud:cmd-finish)
  (user/bind-key-local :debug :continue-until 'realgud:cmd-until)

  ;; Stack frames
  (user/bind-key-local :debug :stack-up 'realgud:cmd-older-frame)
  (user/bind-key-local :debug :stack-down 'realgud:cmd-newer-frame)

  ;; Printing
  (user/bind-key-local :debug :show-value 'realgud:cmd-print))

(use-package gud
  :commands gud-gdb
  :init
  (add-hook 'gud-mode-hook 'user--gud-mode-hook)
  :config
  (validate-setq
   ;; Do not change the working directory before running inferior
   gud-chdir-before-run nil
   ;; Enable tooltips in gud mode
   gud-tooltip-mode t)

  (use-package gdb-mi
    :defer
    :config
    (validate-setq
     ;; Enable many windows
     gdb-many-windows t
     ;; Use a separate buffer for IO
     gdb-show-main t)))

(use-package realgud
  :defer
  :bind-wrap
  (:map c-mode-base-map
        ((:key :debug :start) . realgud-gdb)
   :map go-mode-map
        ((:key :debug :start) . realgud-gub))
  :init
  (add-hook 'realgud-mode-hook 'user/realgud-mode-hook))


(provide 'apps/gud)
;;; gud.el ends here
