;;; compile.el --- sets up Emacs compile support
;;; Commentary:
;;; Code:

(require 'compile)

(defun user/compile-init ()
  "Initialize compile module."
  (setq-default
   compilation-disable-input nil
   compilation-scroll-output t
   mode-compile-always-save-buffer-p t)

  ;;; (Bindings) ;;;
  (define-key user/code-map (kbd "c") 'compile))

(user/compile-init)


(provide 'utilities/compile)
;;; compile.el ends here
