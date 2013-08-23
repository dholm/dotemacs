;;; compile.el --- sets up Emacs compile support
;;; Commentary:
;;; Code:

(defun user/compilation-hook ()
  "Compilation mode hook.")


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
