;;; mail.el --- Major mode for emails -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--mail-mode-hook ()
  "Mail mode hook."
  (setq
   ;; RFC2822 2.1.1 Line Length Limits (including CRLF).
   fill-column (- 78 2)
   ;; Fold lines that are too long.
   truncate-lines nil
   ;; Select abbrev table for mail mode.
   local-abbrev-table mail-mode-abbrev-table)

  ;; For format=flowed support.
  (turn-off-auto-fill)
  (whitespace-mode -1)

  ;; Use org structured editing.
  (orgtbl-mode t)

  ;; Enable footnotes.
  (footnote-mode t)

  ;; Enable BBDB.
  (bbdb-initialize 'mail)

  ;;; (Bindings) ;;;
  (user/bind-key-local :code :try-complete 'user/eudc-expand-inline)
  (user/bind-key-local :code :compile 'org-mime-htmlize))

(use-package sendmail
  :defer
  :init
  (add-hook 'mail-mode-hook 'user--mail-mode-hook)
  :config
  (use-package mu-cite
    :init
    (add-hook 'mail-citation-hook 'mu-cite-original)
    :config
    ;; Undiagnosed issue with validate-setq.
    (setq
     ;; Citation format.
     mu-cite-top-format '("On " date ", " full-name " wrote:\n")
     ;; Use > as prefix.
     mu-cite-prefix-format (quote ("> ")))

    (validate-setq
     ;; Default message citation function.
     message-cite-function 'mu-cite-original)))


(provide 'modes/mail)
;;; mail.el ends here
