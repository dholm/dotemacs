;;; buffers.el --- Configure Emacs buffers
;;; Commentary:
;;; Code:

(defun user/buffers-init ()
  "Initialize Emacs buffers."
  (user/uniquify-init))


(defun user/uniquify-init ()
  "Initialize Emacs unique buffer name creation."
  (require 'uniquify)
  (setq-default
   uniquify-buffer-name-style 'reverse
   uniquify-separator " • "
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*")

  ;;; (Bindings) ;;;
  (global-set-key (kbd "C-x M-s") 'write-file))


(user/buffers-init)


(provide 'ux/buffers)
;;; buffers.el ends here
