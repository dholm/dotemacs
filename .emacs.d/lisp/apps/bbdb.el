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
   bbdb-electric-p t
   ;; Cycle through completions.
   bbdb-complete-name-allow-cycling t
   ;; Always use full name.
   bbdb-dwim-net-address-allow-redundancy t
   bbdb-quiet-about-name-mismatches 2
   ;; Automatically add nress address to existing contact.
   bbdb-always-add-address t
   ;; Hide redundant network names.
   bbdb-canonicalize-redundant-net-p t
   ;; Use cache for better performance.
   bbdb-message-caching-enabled t
   ;; Allow aliases.
   bbdb-use-alternate-names t
   ;; Single-line addresses.
   bbdb-elided-display t
   ;; Ignore certain addresses when adding to address book.
   bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
   bbdb-ignore-some-messages-alist
   '(("From" . "no.*reply\\|DAEMON\\|daemon\\|dacebookmail\\|twitter"))))

(require-package '(:name bbdb :after (user/bbdb-init)))


(provide 'apps/bbdb)
;;; bbdb.el ends here
