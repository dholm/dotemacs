;;; buffers.el --- Configure Emacs buffers
;;; Commentary:
;;; Code:

(defun user/buffers-init ()
  "Initialize Emacs buffers."
  (setq-default
   ;; Set up uniquify's style.
   uniquify-buffer-name-style 'reverse
   uniquify-separator " • "
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*")

  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :open-file 'find-file)
  (user/bind-key-global :basic :open-buffer 'switch-to-buffer)
  (user/bind-key-global :basic :list-buffers 'list-buffers)
  (user/bind-key-global :basic :save 'save-buffer)
  (user/bind-key-global :basic :save-as 'write-file)
  (user/bind-key-global :basic :close 'kill-buffer)
  (user/bind-key-global :basic :quit 'save-buffers-kill-terminal))

(user/buffers-init)


(provide 'ux/buffers)
;;; buffers.el ends here
