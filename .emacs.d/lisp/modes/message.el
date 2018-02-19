;;; message.el --- Major mode for email and news -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--message-mode-hook ()
  "Message mode hook."
  (user--mail-mode-hook)

  (validate-setq
   ;; Select abbrev table for message mode.
   local-abbrev-table message-mode-abbrev-table)

  ;; Enable BBDB.
  (bbdb-initialize 'message)

  (when (feature-p 'google-contacts)
    ;; Google Contacts for message mode.
    (require 'google-contacts-message)))

(defun user--org-mime-html-hook ()
  "Hook for tweaking the HTML output from org-mime."
  ;; Set a dark background for code blocks.
  (org-mime-change-element-style
   "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                 "#E6E1DC" "#232323"))

  ;; Offset block quotes.
  (org-mime-change-element-style
   "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))

(defun user--message-setup-hook ()
  "Outgoing message setup hook."
  ;; Load Emacs directory client.
  (eudc-load-eudc))

(defun user--org-mime-skip-pgp (start)
  "Keep PGP signature outside multipart from START."
  (save-excursion
    (goto-char start)
    (search-forward "<#secure method=pgpmime mode=sign>")
    (+ (point) 1)))

(use-package message
  :ensure nil
  :defer
  :mode ("\.eml$" . message-mode)
  :hook
  ((message-mode-hook . user--message-mode-hook)
   (message-setup-hook . user--message-setup-hook))
  :bind-wrap
  (:map message-mode-map
        ((:key :code :try-complete) . user/eudc-expand-inline)
        ((:key :basic :server-edit) . user/server-save))
  :config
  (validate-setq
   ;; Kill buffer after message is sent.
   message-kill-buffer-on-exit t
   ;; Citation format.
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:"
   ;; Ask for confirmation before sending.
   message-confirm-send t
   ;; Generate headers before editing message.
   message-generate-headers-first t)

  (use-package org-mime
    :pin "MELPA"
    :hook (org-mime-html-hook . user--org-mime-html-hook)
    :bind-wrap
    (:map message-mode-map
          ((:key :code :compile) . org-mime-htmlize))
    :config
    (validate-setq
     ;; Don't include a table of contents.
     org-mime-export-options
     '(:section-numbers nil
                        :with-author nil
                        :with-toc nil)
     ;; Don't modify quoted mail.
     org-mime-beautify-quoted-mail nil
     ;; Be nice to PGP signatures.
     org-mime-find-html-start #'identity)

    (use-package htmlize)))


(provide 'modes/message)
;;; message.el ends here
