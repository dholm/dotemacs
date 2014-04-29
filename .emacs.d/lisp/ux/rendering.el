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
   ;; Defer fontification for faster scrolling in large buffers.
   jit-lock-defer-time 0.05
   ;; Do not reorder text during rendering.
   bidi-display-reordering nil))

(user/rendering-init)


(provide 'ux/rendering)
;;; rendering.el ends here
