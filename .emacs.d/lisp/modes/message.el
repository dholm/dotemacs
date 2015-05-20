;;; message.el --- Major mode for email and news
;;; Commentary:
;;; Code:

(defun user/message-mode-hook ()
  "Message mode hook."
  (setq
   ;; RFC2822 2.1.1 Line Length Limits (including CRLF).
   fill-column (- 78 2)
   ;; Fold lines that are too long.
   truncate-lines nil
   ;; Select abbrev table for message mode.
   local-abbrev-table message-mode-abbrev-table)

  ;; Use org structured editing.
  (orgstruct-mode t)
  (orgstruct++-mode t)
  (orgtbl-mode t)

  ;; Enable footnotes.
  (footnote-mode t)

  ;; Enable BBDB.
  (bbdb-initialize 'message)

  (when (feature-p 'google-contacts)
    ;; Google Contacts for message mode.
    (require 'google-contacts-message))

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :try-complete 'user/eudc-expand-inline)
  (user/bind-key-local :code :compile 'org-mime-htmlize)
  ;; Ensure C-x # is used to save and close message.
  (local-set-key (kbd "C-x #") 'user/server-save))


(defun user/message-send-hook ()
  "Hook run when sending a message."
  ;; Normalize all footnotes in message.
  (org-footnote-normalize))


(defun user/message-setup-hook ()
  "Outgoing message setup hook."
  ;; Load Emacs directory client.
  (eudc-load-eudc))


(defun user/mu-cite-init ()
  "Initialize mu-cite."
  (setq-default
   ;; Citation format.
   mu-cite-top-format '("On " date ", " full-name " wrote:\n")
   ;; Use > as prefix.
   mu-cite-prefix-format (quote ("> "))
   ;; Default message citation function.
   message-cite-function 'mu-cite-original)

  ;;; (Hooks) ;;;
  (add-hook 'mail-citation-hook 'mu-cite-original))


(defun user/message-mode-init ()
  "Initialize message mode."
  (setq-default
   ;; Kill buffer after message is sent.
   message-kill-buffer-on-exit t
   ;; Citation format.
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:"
   ;; Ask for confirmation before sending.
   message-confirm-send t
   ;; Generate headers before editing message.
   message-generate-headers-first t)

  ;; Hooks
  (add-hook 'message-mode-hook 'user/message-mode-hook)
  (add-hook 'message-setup-hook 'user/message-setup-hook)
  (add-hook 'message-send-hook 'user/message-send-hook)

  ;; Register auto mode.
  (add-auto-mode 'message-mode "\\.eml$")

  ;;; (Packages) ;;;
  (require-package '(:name mu-cite :after (user/mu-cite-init))))

(user/message-mode-init)


(provide 'modes/message)
;;; message.el ends here
