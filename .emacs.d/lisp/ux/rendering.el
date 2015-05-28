;;; rendering.el --- Configure Emacs user interface rendering
;;; Commentary:
;;; Code:

(defconst *user-fast-font-lock-buffer-limit* (* 128 1024)
  "Maximum buffer size for maximum font lock decorations.")


(defun user/rendering-find-file-hook ()
  "Rendering hook for `find-file'."
  (when (> (buffer-size) *user-fast-font-lock-buffer-limit*)
    (setq
     ;; Reduce the level of fontification.
     font-lock-maximum-decoration nil)

    (font-lock-refresh-defaults)))


(defun user/rendering-init ()
  "Setup Emacs user interface rendering."
  (after-load 'font-lock
    ;; Allow reduction of decoration level in large buffers.
    (make-variable-buffer-local 'font-lock-maximum-decoration))

  (setq-default
   ;; Redraw the entire screen before checking for pending input events.
   ;; This will improve performance in general but might degrade performance of
   ;; key repeat.
   redisplay-dont-pause t
   ;; Use the maximum amount of decorations by default.
   font-lock-maximum-decoration t
   ;; Always use JIT mode for font-lock.
   font-lock-support-mode 'jit-lock-mode
   ;; Set up font-lock JIT mode to do background parsing.
   jit-lock-stealth-time 1.0
   jit-lock-stealth-nice 0.03
   jit-lock-stealth-load 200
   jit-lock-stealth-verbose nil
   jit-lock-chunk-size 500
   ;; Do not reorder text during rendering.
   bidi-display-reordering nil)

  ;;; (Hooks) ;;;
  (add-hook 'find-file-hook 'user/rendering-find-file-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :emacs :redraw 'redraw-display))

(user/rendering-init)


(provide 'ux/rendering)
;;; rendering.el ends here
