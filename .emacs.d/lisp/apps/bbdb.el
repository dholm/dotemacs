;;; bbdb.el --- Emacs rolodex
;;; Commentary:
;;; Code:

(defconst *user-bbdb-database*
  (path-join *user-data-directory* "bbdb")
  "Path to user's BBDB file.")


(defun user/bbdb-init-hook ()
  "BBDB initialization hook."
  (bbdb-initialize))


(defun user/bbdb-init ()
  "Initialize BBDB."
  (setq-default
   ;; Set up location of database.
   bbdb-file *user-bbdb-database*
   ;; Automatically save seen addresses.
   bbdb-offer-save t
   ;; Use popups for address completion.
   bbdb-use-pop-up t
   ;; Pop up horizontally.
   bbdb-mua-pop-up 'horiz
   bbdb-mua-pop-up-window-size 10
   bbdb-horiz-pop-up-window-size '(80 . 03)
   bbdb-electric-p t
   ;; Cycle through completions.
   bbdb-complete-name-allow-cycling t
   ;; Always use full name.
   bbdb-dwim-net-address-allow-redundancy t
   bbdb-quiet-about-name-mismatches 2
   ;; Automatically add addresses to bbdb.
   bbdb-update-records-p 'create
   bbdb-mua-update-interactive-p '(create . create)
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
   '(("From" . "no.*reply\\|DAEMON\\|daemon")))

  ;; Add notes when updating a record.
  (add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)
  (setq-default bbdb-auto-notes-rules
                (list
                 '("Organization" (".*" organization "\\1" nil))
                 '("User-Agent" (".*" mailer identity nil))
                 '("X-Mailer" (".*" mailer identity nil))
                 '("X-Newsreader" (".*" mailer identity nil)))))

(require-package '(:name bbdb :after (user/bbdb-init)))


(provide 'apps/bbdb)
;;; bbdb.el ends here
