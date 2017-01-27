;;; bbdb.el --- Emacs rolodex
;;; Commentary:
;;; Code:

(defconst *user-bbdb-database*
  (path-join *user-data-directory* "bbdb")
  "Path to user's BBDB file.")


(defun user/bbdb-initialize-hook ()
  "BBDB initialization hook."
  (when (feature-p 'bbdb-vcard)
    ;; Load vCard support.
    (require 'bbdb-vcard)))


(defun user/bbdb-create-or-update-notes ()
  "Auto create records, or update if they exist."
  (let (done elt)
    (while (and (setq elt (pop rest)) (not done))
      (dolist (header (if (stringp (car elt)) (list (car elt)) (car elt)))
        (if (bbdb-message-header-re header (cdr elt))
            (setq done t))))
    (if done 'create 'update)))


(defun user/bbdb-display-record ()
  "Display appropriate BBDB record for the current message."
  (unless
      (bbdb-mua-display-records nil 'search)
    ;; No record found, close the BBDB popup
    (let ((window (get-buffer-window bbdb-buffer-name)))
      (when window (delete-window window)))))


(defun user/bbdb-init ()
  "Initialize BBDB."
  (setq-default
   ;; Set up location of database.
   bbdb-file *user-bbdb-database*
   ;; Automatically save database without asking.
   bbdb-notice-auto-save-file t
   ;; Use popups for address completion.
   bbdb-use-pop-up t
   ;; Try to fit popup horizontally.
   bbdb-mua-pop-up 'horiz
   ;; Size of popup.
   bbdb-popup-target-lines 1
   bbdb-mua-pop-up-window-size 2
   ;; Cycle through completions.
   bbdb-complete-name-allow-cycling t
   ;; Hide pop-up after completion.
   bbdb-completion-display-record nil
   ;; Always use full name.
   bbdb-dwim-net-address-allow-redundancy t
   bbdb-quiet-about-name-mismatches 2
   ;; Automatically add addresses to bbdb.
   bbdb-update-records-p 'create
   bbdb-mua-update-interactive-p '(create . create)
   ;; Don't assume a certain phone number style.
   bbdb-phone-style nil
   ;; Automatically add address to existing contact.
   bbdb-always-add-address t
   ;; Add all addresses in an email.
   bbdb-message-all-addresses t
   ;; Hide redundant network names.
   bbdb-canonicalize-redundant-net-p t
   ;; Use cache for better performance.
   bbdb-message-caching-enabled t
   ;; Allow duplicate entries.
   bbdb-allow-duplicates t
   ;; Allow aliases.
   bbdb-use-alternate-names t
   ;; Single-line addresses.
   bbdb-elided-display t
   ;; Ignore certain addresses when adding to address book.
   bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
   bbdb-ignore-some-messages-alist
   '(("From" . "no.*reply\\|DAEMON\\|daemon"))
   ;; Fields to auto-populate in notes.
   bbdb-auto-notes-rules
   (list
    '("Organization" (".*" organization "\\1" nil))
    '("Newsgroups" ("[^,]+" newsgroups identity nil))
    '("Xref" ("[^ ]+ \\([^ :]+\\):[0-9]+" newsgroups "\\1" nil))
    '("User-Agent" (".*" mailer identity nil))
    '("X-Mailer" (".*" mailer identity nil))
    '("X-Newsreader" (".*" mailer identity nil)))
   bbdb-update-records-p 'user/bbdb-create-or-update-notes)

  ;;; (Hooks) ;;;
  ;; Add notes when updating a record.
  (add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)
  (add-hook 'bbdb-initialize-hook 'user/bbdb-initialize-hook))

(use-package bbdb
  :defer t
  :config (user/bbdb-init))
(use-package bbdb-vcard
  :defer t)


(provide 'apps/bbdb)
;;; bbdb.el ends here
