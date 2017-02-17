;;; wanderlust.el --- wanderlust mail and news management system
;;; Commentary:
;;; Code:

(defconst *user-wanderlust-data-directory*
  (path-join *user-data-directory* "wanderlust")
  "Path to user's Wanderlust data store.")

(defconst *user-wanderlust-cache-directory*
  (path-join *user-cache-directory* "wanderlust")
  "Path to user's Wanderlust cache store.")


(defun user--wl-folder-mode-hook ()
  "Wanderlust folder mode hook."
  (hl-line-mode t))


(defun user--wl-summary-mode-hook ()
  "Wanderlust summary mode hook."
  (hl-line-mode t))


(defun user--wl-draft-mode-hook ()
  "Wanderlust draft mode hook.")


(defun user--mime-view-mode-hook ()
  "Wanderlust mime view mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :nav :open 'mime-preview-toggle-content))


(defun user--mime-edit-mode-hook ()
  "Wanderlust mime edit mode hook.")


(defun user--wl-message-buffer-created-hook ()
  "Wanderlust message buffer created hook."
  (setq
   ;; Fold lines that are too long.
   truncate-lines nil))


(defun user--wanderlust-notify-hook ()
  "Wanderlust email notification hook."
  (cond
   ((feature-p 'alert)
    (alert "You've got mail." :severity 'trivial))
   (t (ding))))


(defun user--wl-message-redisplay-hook ()
  "Wanderlust message redisplay hook."
  (when (display-graphic-p)
    (smiley-region (point-min) (point-max))))


(defun user--wl-mail-setup-hook ()
  "Setup hook for new emails."
  ;; Pick email account template when opening a draft.
  (add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)

  ;; Set format to flowed.
  (save-excursion
    (mail-text)
    (mime-edit-insert-tag "text" "plain" "; format=flowed")))


(defun user--wl-config-hook ()
  "Wanderlust initialization hook."
  ;; Load Emacs directory client.
  (eudc-load-eudc)

  (with-feature 'bbdb
    (bbdb-initialize))

  (when (feature-p 'org-mode)
    (with-feature 'org-mime
      (validate-setq
       org-mime-library 'semi)))

  ;; Set up wanderlust as the default mail user agent.
  (when (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))

  (when (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

  ;;; (Bindings) ;;;
  (define-key wl-draft-mode-map "\C-l" nil)
  ;; Cite original email by default.
  (define-key wl-summary-mode-map (kbd "A") 'wl-summary-reply)
  (define-key wl-summary-mode-map (kbd "a") 'wl-summary-reply-with-citation))


(defun user/icalendar-import-mime-text (entity &optional situation)
  "Import calendar from ENTITY to org agenda, SITUATION is ignored."
  (save-excursion
    (let ((calendar-temp-file
           (make-temp-file
            (path-join *user-wanderlust-cache-directory* "import.cal"))))
      (mime-write-entity-content entity calendar-temp-file)
      (icalendar-import-file calendar-temp-file diary-file)
      (kill-buffer (find-buffer-visiting calendar-temp-file))
      (delete-file calendar-temp-file))))


(defun wl-summary-overview-entity-compare-by-reply-date (a b)
  "Compare message A and B by latest date of replies including thread."
  (flet ((string-max2 (x y)
          (cond ((string< x y) y)
                ('t x)))
         (thread-number-get-date (x)
          (timezone-make-date-sortable (elmo-msgdb-overview-entity-get-date
                                        (elmo-message-entity
                                         wl-summary-buffer-elmo-folder x))))
         (thread-get-family (x)
          (cons x (wl-thread-entity-get-descendant (wl-thread-get-entity x))))
         (max-reply-date (x)
          (cond ((eq 'nil x)
                 'nil)
                ((eq 'nil (cdr x))
                 (thread-number-get-date (car x)))
                ('t
                 (string-max2 (thread-number-get-date (car x))
                              (max-reply-date (cdr x)))))))
    (string<
     (max-reply-date (thread-get-family (elmo-message-entity-number a)))
     (max-reply-date (thread-get-family (elmo-message-entity-number b))))))


(defun user--mime-display-text/plain-hook ()
  "Plain text display hook."
  (let ((fmt (cdr (assoc "format" (mime-content-type-parameters
                                   (mime-entity-content-type entity))))))
    (when (string= "flowed" fmt)
      (fill-flowed))))


(defun user--semi-shr-config ()
  "Configure SEMI to use SHR."
  (autoload 'mime-shr-preview-text/html "mime-shr")

  ;; Use shr to view HTML mail.
  (ctree-set-calist-strictly
   'mime-preview-condition
   '((type . text)
     (subtype . html)
     (body . visible)
     (body-presentation-method . mime-shr-preview-text/html))))


(defun user--semi-w3m-config ()
  "Configure SEMI to use w3m."
  (autoload 'mime-w3m-preview-text/html "mime-w3m")

  ;; Use w3m to view HTML mail.
  (ctree-set-calist-strictly
   'mime-preview-condition
   '((type . text)
     (subtype . html)
     (body . visible)
     (body-presentation-method . mime-w3m-preview-text/html))))


(defun user--semi-config ()
  "Initialize SEMI."
  (validate-setq
   ;; Don't split large mails.
   mime-edit-split-message nil
   ;; Decrypt encrypted emails automatically.
   mime-pgp-decrypt-when-preview t
   ;; MIME type priorities.
   mime-view-type-subtype-score-alist
   '(((text . plain) . 4)
     ((text . enriched) . 3)
     ((text . html) . 2)
     ((text . richtext) . 1)))

  (after-load 'mime-view
    (cond
     ((feature-p 'mime-shr) (user--semi-shr-config))
     ((feature-p 'emacs-w3m) (user--semi-w3m-config)))

    (set-alist 'mime-view-type-subtype-score-alist '(text . html) 3)

    (ctree-set-calist-strictly
     'mime-acting-condition
     '((mode . "play")
       (type . text)
       (subtype . calendar)
       (method . user/icalendar-import-mime-text))))

  (add-hook 'mime-view-mode-hook 'user--mime-view-mode-hook)
  (add-hook 'mime-edit-mode-hook 'user--mime-edit-mode-hook))


(defun user/wanderlust-set-gmail-user (fullname username)
  "Configure Wanderlust to use \"FULLNAME\" <USERNAME@gmail.com>."
  (let ((email-address (concat username "@gmail.com"))
        (folder-template (format "\"%s@gmail.com\"/clear@imap.gmail.com:993!" username)))
    (unless (file-exists-p wl-folders-file)
      (user/wanderlust-create-folders-gmail username wl-folders-file))

    ;; Register email address.
    (add-to-list 'wl-user-mail-address-list email-address)

    ;; Set up account template.
    (add-to-list
     'wl-template-alist
     `(,email-address
       (wl-from . ,(concat fullname " <" email-address ">"))
       ("From" . wl-from)
       (wl-insert-message-id . nil)
       (wl-local-domain . "gmail.com")
       (wl-message-id-domain . "smtp.gmail.com")
       ;; SMTP
       (wl-smtp-posting-user . ,username)
       (wl-smtp-authenticate-type . "plain")
       (wl-smtp-connection-type . 'starttls)
       (wl-smtp-posting-port . 587)
       (wl-smtp-posting-server . "smtp.gmail.com")
       ;; Folders
       (wl-default-spec . "%")
       (wl-default-folder . "%inbox")
       (wl-draft-folder . "%[Gmail]/Drafts")
       (wl-spam-folder . "%[Gmail]/Spam")
       (wl-trash-folder . "%[Gmail]/Trash")
       (wl-fcc . "%[Gmail]/Sent")))

    ;; Point draft configuration to correct template.
    (add-to-list
     'wl-draft-config-alist
     `((string-match ,email-address  wl-draft-parent-folder)
       (template . ,email-address)))

    ;; Set up trash folder for account.
    (add-to-list
     'wl-dispose-folder-alist
     `(,email-address . ,(concat "%[Gmail]/Trash" folder-template)))

    ;; Check inbox for new mail.
    (add-to-list 'wl-biff-check-folder-list
                 (concat "%inbox" folder-template))))


(defun user/wanderlust-create-folders-gmail (username folders-file)
  "Write GMail folders for USERNAME@gmail.com to FOLDERS-FILE."
  (let ((server-string (format "\"%s@gmail.com\"/clear@imap.gmail.com:993!" username))
        (folders-template "# -*- conf-unix -*-
+trash  \"Trash\"
+draft  \"Drafts\"

Gmail {
   %inbox:SERVER \"Inbox\"
   Labels {
       %[Gmail]/Important:SERVER \"Important\"
       %[Gmail]/Drafts:SERVER \"Drafts\"
       %[Gmail]/All Mail:SERVER \"All Mail\"
       %[Gmail]/Sent Mail:SERVER \"Sent Mail\"
       %[Gmail]/Starred:SERVER \"Starred\"
       %[Gmail]/Spam:SERVER \"Spam\"
       %[Gmail]/Trash:SERVER \"Trash\"
   }
}
"))
    (with-temp-buffer
      (insert
       (replace-regexp-in-string "SERVER" server-string folders-template t))
      (when (file-writable-p folders-file)
        (write-region (point-min) (point-max) folders-file)))))


(defun user/wl-summary-line-attached ()
  "Summary-line formatter for attachments."
  (let ((content-type (or (elmo-message-entity-field
                           wl-message-entity 'content-type) ""))
        (case-fold-search t))
    (cond
     ((string-match "multipart/mixed" content-type) "@")
     ((string-match "multipart/encrypted" content-type) "?")
     ((string-match "multipart/signed" content-type) "#")
     ((string-match "multipart/" content-type) "%")
     (t ""))))


(defun user/wl-summary-line-where-is-my-address ()
  "Summary-line formatter for mail explicitly including user."
  (cond
   ((catch 'found
      (dolist (cc (elmo-message-entity-field wl-message-entity 'cc))
        (when (wl-address-user-mail-address-p cc)
          (throw 'found t))))
    "Cc")
   ((catch 'found
      (dolist (to (elmo-message-entity-field wl-message-entity 'to))
        (when (wl-address-user-mail-address-p to)
          (throw 'found t))))
    "To")
   ((wl-address-user-mail-address-p
     (elmo-message-entity-field wl-message-entity 'from))
    "From")
   (t "")))


(defun user/wanderlust-set-summary-guides ()
  "Set Wanderlust summary thread guides."
  (if (eq default-terminal-coding-system 'utf-8)
      (validate-setq
       wl-thread-indent-level 2
       wl-thread-have-younger-brother-str "├─┬─"
       wl-thread-vertical-str             "│"
       wl-thread-youngest-child-str       "╰───"
       wl-thread-horizontal-str           "─"
       wl-thread-space-str                " ")
    (validate-setq
     wl-thread-indent-level 2
     wl-thread-have-younger-brother-str "+"
     wl-thread-youngest-child-str       "+"
     wl-thread-vertical-str             "|"
     wl-thread-horizontal-str           "-"
     wl-thread-space-str                " ")))


(defun user/wanderlust-summary-refile (&optional folder)
  "Refile the current message to FOLDER; if FOLDER is nil, use the default."
  (interactive)
  (wl-summary-refile (wl-summary-message-number) folder)
  (wl-summary-next)
  (message "%s" (concat "Refiled to " folder)))


(use-package wanderlust
  :defer t
  :init
  ;; Create data and cache stores.
  (make-directory *user-wanderlust-data-directory* t)
  (set-file-modes *user-wanderlust-data-directory* #o0700)
  (make-directory *user-wanderlust-cache-directory* t)
  (set-file-modes *user-wanderlust-cache-directory* #o0700)
  ;;; (Hooks) ;;;
  (add-hook 'wl-init-hook 'user--wl-config-hook)
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :email 'wl)  :config)

(use-package wl
  :after wanderlust
  :config
  (with-feature 'fullframe
    (fullframe wl wl-exit nil))
  ;;; (Hooks) ;;;
  ;; Support for writing flowed emails.
  (add-hook 'wl-draft-mode-hook 'user--wl-draft-mode-hook))

(use-package wl-vars
  :after wanderlust
  :config
  (validate-setq
   ;;; (Basic Configuration) ;;;
   ;; Put configuration into wanderlust data directory.
   wl-init-file (path-join *user-wanderlust-data-directory* "init.el")
   wl-folders-file (path-join *user-wanderlust-data-directory* "folders")
   wl-address-file (path-join *user-wanderlust-data-directory* "addresses")
   ;; Put temporary files in cache directories.
   wl-temporary-file-directory *user-wanderlust-cache-directory*
   ;;; (Wanderlust) ;;;
   ;; Mark sent mails as read.
   wl-fcc-force-as-read t
   ;; Check for mail when idle.
   wl-biff-check-interval 180
   wl-biff-use-idle-timer t
   ;; Set notification function.
   wl-biff-notify-hook 'user--wanderlust-notify-hook
   ;; Let SMTP server handle Message-ID.
   wl-insert-message-id nil
   ;; Quit without asking.
   wl-interactive-exit nil
   ;;; (Modeline) ;;;
   ;; Show mail status in mode line.
   global-mode-string (cons '(wl-modeline-biff-status
                              wl-modeline-biff-state-on
                              wl-modeline-biff-state-off) global-mode-string)
   ;;; (Messages) ;;;
   ;; Message window size.
   wl-message-window-size '(1 . 3)
   ;; Field lists.
   wl-message-ignored-field-list '("^.*")
   wl-message-visible-field-list '("^\\(From\\|Reply-To\\):" "^\\(To\\|Cc\\):"
                                   "^Organization:" "^Subject:"
                                   "^X-Attribution:" "^\\(Posted\\|Date\\):"
                                   "^\\(Posted\\|Date\\):"
                                   "^\\(User-Agent\\|X-Mailer\\):")
   ;; Allow sort on visible fields.
   wl-message-sort-field-list wl-message-visible-field-list
   ;;; (Drafts) ;;;
   ;; Raise a new frame when creating a draft.
   wl-draft-use-frame t
   ;; Automatically save drafts every two minutes.
   wl-auto-save-drafts-interval 120.0
   ;; Sane forward tag.
   wl-forward-subject-prefix "Fwd: "
   ;; Automatically select the correct template based on folder.
   wl-draft-config-matchone t
   ;;; (Summary) ;;;
   ;; Set verbose summary.
   wl-summary-width nil
   wl-summary-line-format
   "%T%P │%Y-%M-%D %h:%m│ %17(%f%) │%-4S│%4i│%1@ %t%~%c%~%#%~%s "
   wl-folder-summary-line-format-alist
   '(("^+" . "%n%T%P │%Y-%M-%D %h:%m│ %17(%f%) │%-4S││%4i│%1@ %t%~%c%~%#%~%s ")
     ("^file:" . "%T%P %17f %-5S %Y/%M/%D(%W) %h:%m %s "))
   ;; Format of mode-line entry.
   wl-summary-mode-line-format "WL:%n/%u/%a{%t}%f"
   ;; Display TO rather than FROM in "Sent" folders.
   wl-summary-showto-folder-regexp ".*Sent.*"
   ;; Divide thread if subject has changed.
   wl-summary-divide-thread-when-subject-changed t
   ;; List of marks to display in summary.
   wl-summary-incorporate-marks '("N" "U" "!" "A" "$")
   ;; (Folders) ;;
   ;; Show folders in a pane to the left.
   wl-stay-folder-window nil
   wl-folder-window-width 45
   ;; Asynchronously update folders.
   wl-folder-check-async t)

  (add-many-to-list
   'wl-summary-line-format-spec-alist
   ;; Email to user formatter.
   '(?i (user/wl-summary-line-where-is-my-address))
   ;; Attachment formatter.
   '(?@ (user/wl-summary-line-attached)))

  (when (display-graphic-p)
    (with-feature 'wl-gravatar
      (validate-setq
       ;; Insert gravatar as email X-Face.
       wl-highlight-x-face-function 'wl-gravatar-insert)))

  ;;; (Hooks) ;;;
  (add-hook 'wl-message-redisplay-hook 'user--wl-message-redisplay-hook)
  (add-hook 'wl-message-buffer-created-hook
            'user--wl-message-buffer-created-hook)
  ;;; (Drafts) ;;;
  (add-hook 'wl-mail-setup-hook 'user--wl-mail-setup-hook)
  ;; Don't apply email account template when sending draft, otherwise switching
  ;; templates won't work.
  (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)
  ;;; (Summary) ;;;
  (add-hook 'wl-folder-mode-hook 'user--wl-folder-mode-hook)
  (add-hook 'wl-summary-mode-hook 'user--wl-summary-mode-hook))

(use-package wl-summary
  :after wl
  :config
  ;; Sort threads based on the date of the latest reply.
  (add-to-list 'wl-summary-sort-specs 'reply-date)
  (setq wl-summary-default-sort-spec 'reply-date)

  ;; Set up guides in summary mode.
  (user/wanderlust-set-summary-guides))

(use-package semi
  :after wanderlust
  :config (user--semi-config))

(use-package mime-view
  :after semi
  :config
  ;; Support for reading flowed emails.
  (add-hook 'mime-display-text/plain-hook 'user--mime-display-text/plain-hook))

(use-package elmo
  :after wanderlust
  :config
  (validate-setq
   ;; Maximum size of message to fetch without confirmation.
   elmo-message-fetch-threshold (* 512 1024))

  (unless (executable-find "namazu")
    (message "Namazu not found, mail will not be indexed.")))

(use-package elmo-vars
  :after elmo
  :config
  (validate-setq
   ;; Put message database in data directory.
   elmo-msgdb-directory (path-join *user-wanderlust-data-directory* "elmo")
   ;; ELMO's cache go into the user cache directory.
   elmo-cache-directory (path-join *user-wanderlust-cache-directory* "elmo")))

(use-package elmo-archive
  :after elmo
  :config
  (validate-setq
   elmo-archive-folder-path *user-wanderlust-data-directory*))

(use-package elmo-search
  :after elmo
  :config
  (validate-setq
   elmo-search-namazu-default-index-path *user-wanderlust-data-directory*))

(use-package elmo-localdir
  :after elmo
  :config
  (validate-setq
   elmo-localdir-folder-path *user-wanderlust-data-directory*))

(use-package elmo-maildir
  :after elmo
  :config
  (validate-setq
   elmo-maildir-folder-path *user-wanderlust-data-directory*))

(use-package elmo-imap4
  :after elmo
  :config
  (validate-setq
   ;; Use modified UTF-8 for IMAP4.
   elmo-imap4-use-modified-utf7 t))

(use-package wl-gravatar
  :if window-system
  :requires wanderlust
  :quelpa (wl-gravatar :fetcher github :repo "dabrahams/wl-gravatar")
  :init
  (autoload 'wl-gravatar-insert "wl-gravatar"))


(provide 'apps/wanderlust)
;;; wanderlust.el ends here
