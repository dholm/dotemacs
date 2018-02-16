;;; mu4e.el --- mu4e mail management system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--mu4e-search-for-sender (msg)
  "Search for messages from sender in MSG."
  (mu4e-headers-search
   (concat "from:" (cdar (mu4e-message-field msg :from)))))

(use-package mu4e
  :ensure nil
  :el-get t
  :commands mu4e
  :config
  (use-package mu4e-vars
    :ensure nil
    :config
    (validate-setq
     ;; Skip confirmations.
     mu4e-confirm-quit nil
     ;; Attachments.
     mu4e-attachment-dir (path-join *user-home-directory* "Downloads")
     mu4e-completing-read-function 'helm--completing-read-default
     ;; Automatically retrieve public keys.
     mu4e-auto-retrieve-keys t)

    ;; Bookmarks
    (add-to-list 'mu4e-bookmarks
                 '("flag:flagged" "Flagged" ?f))
    (add-to-list 'mu4e-bookmarks
                 '("date:1h..now" "Last hour" ?h))
    (add-to-list 'mu4e-bookmarks
                 '("flag:unread AND NOT flag:trashed AND NOT list:*"
                   "Unread messages, no lists" ?U))
    (add-to-list 'mu4e-bookmarks
                 '("flag:attach" "With attachments" ?a) t)
    (add-to-list 'mu4e-bookmarks
                 '("mime:application/pdf" "With documents" ?d) t)
    (add-to-list 'mu4e-bookmarks
                 '("flag:unread AND NOT flag:trashed AND list:*"
                   "Unread lists" ?l))

    (when (eq default-terminal-coding-system 'utf-8)
      (validate-setq
       ;; Allow non-ASCII characters.
       mu4e-use-fancy-chars t)))

  (use-package mu4e-headers
    :ensure nil
    :config
    (validate-setq
     ;; Include related messages in search.
     mu4e-headers-include-related t
     ;; Hide duplicates in search.
     mu4e-headers-skip-duplicates t
     ;; Fields in header.
     mu4e-headers-fields
     '((:human-date . 10)
       (:flags . 5)
       (:mailing-list . 15)
       (:size . 6)
       (:from-or-to . 22)
       (:subject)))

    (when (eq default-terminal-coding-system 'utf-8)
      (validate-setq
       ;; Shorter prefixes.
       mu4e-headers-from-or-to-prefix '("" . "➜ ")
       ;; Representations of marks.
       mu4e-headers-attach-mark    '("a" . "✇")
       mu4e-headers-new-mark       '("N" . "★")
       mu4e-headers-draft-mark     '("D" . "✍")
       mu4e-headers-encrypted-mark '("x" . "⚷")
       mu4e-headers-flagged-mark   '("F" . "⚑")
       mu4e-headers-trashed-mark   '("T" . "♻")
       mu4e-headers-seen-mark      '("S" . "☑")
       mu4e-headers-unread-mark    '("u" . "☐")
       ;; Thread prefix marks.
       mu4e-headers-empty-parent-prefix '("-" . "──▶")
       mu4e-headers-default-prefix      '("|" . "   ")
       mu4e-headers-has-child-prefix    '("+" . "╰┬▶")
       mu4e-headers-first-child-prefix  '("\\" . "╰─▶")
       mu4e-headers-duplicate-prefix    '("=" . "≡  "))))

  (use-package mu4e-view
    :ensure nil
    :config
    (validate-setq
     ;; Show name and address.
     mu4e-view-show-addresses t
     ;; Show images by default in graphic mode.
     mu4e-view-show-images (display-graphic-p))
    (setq
     ;; Message view buffer headers.
     mu4e-view-fields
     '(:from
       :to
       :cc
       :bcc
       :subject
       :flags
       :date
       :maildir
       :mailing-list
       :tags
       :attachments
       :signature
       :decryption))

    ;; Add additional view actions.
    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-action-view-in-browser) t)
    (add-to-list 'mu4e-view-actions
                 '("Retag email" . mu4e-action-retag-message) t)
    ;; define 'x' as the shortcut
    (add-to-list 'mu4e-view-actions
                 '("Search for sender" . joe-search-for-sender) t)

    (when (fboundp 'imagemagick-register-types)
      ;; Enable imagemagick support.
      (imagemagick-register-types)))

  (use-package mu4e-draft
    :ensure nil
    :config
    (validate-setq
     ;; Don't include signature by default.
     mu4e-compose-signature-auto-include nil))

  (use-package mu4e-contrib
    :ensure nil)

  (use-package org-mu4e
    :ensure nil
    :config
    (validate-setq
     ;; Don't automatically convert to HTML on send.
     org-mu4e-convert-to-html nil))

  (use-package mu4e-query-fragments)
  (use-package mu4e-jump-to-list)

  (use-package mu4e-alert
    :config
    (validate-setq
     ;; Mail filter for triggering alert.
     mu4e-alert-interesting-mail-query
     "flag:unread AND NOT flag:list")

    ;; Enable alerts.
    (mu4e-alert-enable-notifications)))


(provide 'apps/mu4e)
;;; mu4e.el ends here
