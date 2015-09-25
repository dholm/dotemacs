;;; mail.el --- Major mode for emails
;;; Commentary:
;;; Code:

(defun user/mail-mode-hook ()
  "Mail mode hook."
  (setq
   ;; RFC2822 2.1.1 Line Length Limits (including CRLF).
   fill-column (- 78 2)
   ;; Fold lines that are too long.
   truncate-lines nil
   ;; Select abbrev table for mail mode.
   local-abbrev-table mail-mode-abbrev-table)

  ;; Use org structured editing.
  (orgstruct-mode t)
  (orgtbl-mode t)

  ;; Enable footnotes.
  (footnote-mode t)

  ;; Enable BBDB.
  (bbdb-initialize 'mail)

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :try-complete 'user/eudc-expand-inline)
  (user/bind-key-local :code :compile 'org-mime-htmlize))


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


(defun user/mail-mode-init ()
  "Initialize mail mode."
  ;; Hooks
  (add-hook 'mail-mode-hook 'user/mail-mode-hook)

  ;;; (Packages) ;;;
  (require-package '(:name mu-cite :after (user/mu-cite-init))))

(user/mail-mode-init)


(provide 'modes/mail)
;;; mail.el ends here
