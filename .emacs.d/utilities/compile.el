;;; compile.el --- sets up Emacs compile support
;;; Commentary:
;;; Code:

(defun user/compilation-hook ()
  "Compilation mode hook."
  ;; Create a new compilation window if it isn't open
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        ;; Split vertically and set window to 15 lines high
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h 15)))))))


(defun user/compile-init ()
  "Initialize compile module."
  (setq-default
   ;; Prevent input in compilation buffer
   compilation-disable-input nil
   ;; Automatically scroll output
   compilation-scroll-output t
   ;; Save the current buffer on compilation
   mode-compile-always-save-buffer-p t)

  ;; Add compilation mode hook
  (add-hook 'compilation-mode-hook 'user/compilation-hook)

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "c") 'compile))

(user/compile-init)


(provide 'utilities/compile)
;;; compile.el ends here
