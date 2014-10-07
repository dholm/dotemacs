;;; rendering.el --- Configure Emacs user interface rendering
;;; Commentary:
;;; Code:

(defun user/rendering-init ()
  "Setup Emacs user interface rendering."
  (setq-default
   ;; Update Emacs display every ten seconds when idle.
   idle-update-delay 10
   ;; Redraw the entire screen before checking for pending input events.
   ;; This will improve performance in general but might degrade performance of
   ;; key repeat.
   redisplay-dont-pause t
   ;; Always use JIT mode for font-lock.
   font-lock-support-mode 'jit-lock-mode
   ;; Set up font-lock JIT mode to do background parsing.
   jit-lock-stealth-time 1.0
   jit-lock-stealth-nice 0.03
   jit-lock-stealth-load 200
   jit-lock-stealth-verbose nil
   jit-lock-chunk-size 500
   ;; Do not reorder text during rendering.
   bidi-display-reordering nil))

(user/rendering-init)


(provide 'ux/rendering)
;;; rendering.el ends here
