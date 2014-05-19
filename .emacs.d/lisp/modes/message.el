;;; message.el --- Major mode for email and news
;;; Commentary:
;;; Code:

(defun user/message-mode-hook ()
  "Message mode hook."
  (setq
   ;; RFC2822 2.1.1 Line Length Limits.
   fill-column 78
   ;; Fold lines that are too long.
   truncate-lines nil
   ;; Select abbrev table for message mode.
   local-abbrev-table message-mode-abbrev-table)

  ;; Use org structured editing.
  (turn-on-orgstruct)
  (turn-on-orgstruct++)

  ;; Enable footnotes.
  (footnote-mode t)

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :try-complete 'bbdb-complete-name)
  (user/bind-key-local :code :compile 'org-mime-htmlize)
  ;; Ensure C-c C-c is used to save and close message.
  (local-set-key (kbd "C-c C-c") 'user/server-save))


(defun user/message-mode-init ()
  "Initialize message mode."
  (setq-default
   ;; Kill buffer after message is sent.
   message-kill-buffer-on-exit t)

  (add-hook 'message-mode-hook 'user/message-mode-hook)

  ;; Register auto mode.
  (add-auto-mode 'message-mode "\\.eml$"))

(user/message-mode-init)


(provide 'modes/message)
;;; message.el ends here
