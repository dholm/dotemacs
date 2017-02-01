;;; bbdb.el --- Emacs rolodex
;;; Commentary:
;;; Code:

(defconst *user-bbdb-database*
  (path-join *user-data-directory* "bbdb")
  "Path to user's BBDB file.")


(defun user--bbdb-configialize-hook ()
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


(defun user--bbdb-config ()
  "Initialize BBDB."
  (validate-setq
   ;; Set up location of database.
   bbdb-file *user-bbdb-database*
   ;; Automatically save database without asking.
   bbdb-check-auto-save-file t
   ;; Try to fit popup horizontally.
   bbdb-mua-pop-up 'horiz
   ;; Size of popup.
   bbdb-mua-pop-up-window-size 2
   ;; Cycle through completions.
   bbdb-complete-mail-allow-cycling t
   ;; Hide pop-up after completion.
   bbdb-completion-display-record nil
   ;; Automatically add addresses to bbdb.
   bbdb-update-records-p 'create
   bbdb-mua-update-interactive-p '(create . create)
   ;; Don't assume a certain phone number style.
   bbdb-phone-style nil
   ;; Add all addresses in an email.
   bbdb-message-all-addresses t
   ;; Allow duplicate entries.
   bbdb-allow-duplicates t
   ;; Allow aliases.
   bbdb-add-aka t
   ;; Ignore certain addresses when adding to address book.
   bbdb-ignore-message-alist
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
  (add-hook 'bbdb-initialize-hook 'user--bbdb-configialize-hook))

(use-package bbdb
  :ensure t
  :commands (bbdb-initialize)
  :config (user--bbdb-config))
(use-package bbdb-vcard
  :after bbdb)


(provide 'apps/bbdb)
;;; bbdb.el ends here
