;;; wanderlust.el --- wanderlust mail and news management system
;;; Commentary:
;;; Code:

(defconst *user-wanderlust-data-directory*
  (path-join *user-data-directory* "wanderlust")
  "Path to user's Wanderlust data store.")

(defconst *user-wanderlust-cache-directory*
  (path-join *user-cache-directory* "wanderlust")
  "Path to user's Wanderlust cache store.")


(defun user/mail-citation-hook ()
  "Mail citation hook."
  (sc-cite-original))


(defun user/bbdbv3-wl-init-hook ()
  "BBDBv3 Wanderlust initiatlization hook."
  (user/bbdb-init-hook)
  (require 'bbdbV3-wl))


(defun user/wl-init-hook ()
  "Wanderlust initialization hook."
  (when (el-get-package-is-installed 'bbdbv3-wl)
    (user/bbdbv3-wl-init-hook))

  (when (el-get-package-is-installed 'org-mode)
    (require 'org-mime)
    (setq-default
     org-mime-library 'semi))

  ;; Set up wanderlust as the default mail user agent.
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))
  (add-hook 'mail-citation-hook 'user/mail-citation-hook))


(defun user/semi-init ()
  "Initialize SEMI."
  (setq-default
   ;; MIME type priorities.
   mime-view-type-subtype-score-alist '(((text . plain) . 4)
                                        ((text . enriched) . 3)
                                        ((text . html) . 2)
                                        ((text . richtext) . 1))
   ;; Don't split large mails.
   mime-edit-split-message nil)

  (after-load 'mime-view
    (autoload 'mime-w3m-preview-text/html "mime-w3m")
    (ctree-set-calist-strictly
     'mime-preview-condition
     '((type . text)
       (subtype . html)
       (body . visible)
       (body-presentation-method . mime-w3m-preview-text/html)))
    (set-alist 'mime-view-type-subtype-score-alist '(text . html) 3)))


(defun user/elmo-init ()
  "Initialize ELMO."
  (setq-default
   ;; Put message database in data directory.
   elmo-msgdb-directory (path-join *user-wanderlust-data-directory* "elmo")
   ;; Folders go in data directory too.
   elmo-localdir-folder-path *user-wanderlust-data-directory*
   elmo-maildir-folder-path *user-wanderlust-data-directory*
   elmo-search-namazu-default-index-path *user-wanderlust-data-directory*
   elmo-archive-folder-path *user-wanderlust-data-directory*
   ;; ELMO's cache go into the user cache directory.
   elmo-cache-directory (path-join *user-wanderlust-cache-directory* "elmo")
   ;; Use modified UTF-8 for IMAP4.
   elmo-imap4-use-modified-utf7 t)

  (unless (executable-find "namazu")
    (message "Namazu not found, mail will not be indexed.")))


(defun user/supercite-init ()
  "Initialize supercite."
  (setq-default
   sc-citation-leader ""
   ;; Use nested citations.
   sc-nested-citation-p t
   ;; Do not confirm attribution string before citation.
   sc-confirm-always-p nil))


(defun user/wl-folder-mode-hook ()
  "Wanderlust folder mode hook."
  (hl-line-mode t))


(defun user/wl-summary-mode-hook ()
  "Wanderlust summary mode hook."
  (hl-line-mode t))


(defun user/wl-draft-mode-hook ()
  "Wanderlust draft mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :complete 'bbdb-complete-name)
  (user/bind-key-local :code :compile 'org-mime-htmlize))


(defun user/wl-message-buffer-created-hook ()
  "Wanderlust message buffer created hook."
  (setq
   ;; Fold lines that are too long.
   truncate-lines nil))


(defun user/wl-message-redisplay-hook ()
  "Wanderlust message redisplay hook."
  (when (display-graphic-p)
    (smiley-region (point-min) (point-max))))


(defun user/wanderlust-set-gmail-user (fullname username)
  "Configure Wanderlust to use \"FULLNAME\" <USERNAME@gmail.com>."
  (let ((email-address (concat username "@gmail.com"))
        (folder-template (format ":%s/clear@imap.gmail.com:993!" username)))
    (unless (file-exists-p wl-folders-file)
      (user/wanderlust-create-folders-gmail username wl-folders-file))

    ;; Register email address.
    (add-to-list 'wl-user-mail-address-list email-address)

    ;; Set up account template.
    (add-to-list
     'wl-template-alist
     `((,email-address
        (wl-from . ,(concat fullname " <" email-address ">"))
        ("From" . wl-from)
        (wl-insert-message-id . nil)
        (wl-local-domain . "gmail.com")
        (wl-message-id-domain . "smtp.gmail.com")
        ;; IMAP
        (elmo-imap4-default-server . "imap.gmail.com")
        (elmo-imap4-default-user . ,email-address)
        (elmo-imap4-default-authenticate-type . 'clear)
        (elmo-imap4-default-port . '993)
        (elmo-imap4-default-stream-type . 'ssl)
        ;; SMTP
        (wl-smtp-posting-user . ,username)
        (wl-smtp-authenticate-type . 'plain)
        (wl-smtp-connection-type . 'starttls)
        (wl-smtp-posting-port . 587)
        (wl-smtp-posting-server . "smtp.gmail.com")
        ;; Folders
        (wl-default-spec . "%")
        (wl-default-folder . "%inbox")
        (wl-draft-folder . "%[Gmail]/Drafts")
        (wl-spam-folder . "%[Gmail]/Spam")
        (wl-trash-folder . "%[Gmail]/Trash")
        (wl-fcc . "%[Gmail]/Sent"))))

    ;; Point draft configuration to correct template.
    (add-to-list
     'wl-draft-config-alist
     `(((string-match ,email-address  wl-draft-parent-folder)
        (template . ,email-address))))

    ;; Set up trash folder for account.
    (add-to-list
     'wl-dispose-folder-alist
     `((,email-address . ,(concat "%[Gmail]/Trash" folder-template))))

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


(defun user/wanderlust-init ()
  "Initialize Wanderlust."
  (el-get-eval-after-load 'semi
    (user/semi-init))
  (after-load 'supercite
    (user/supercite-init))
  (after-load 'elmo
    (user/elmo-init))

  (setq-default
   ;;; (Basic Configuration) ;;;
   ;; Put configuration into wanderlust data directory.
   wl-init-file (path-join *user-wanderlust-data-directory* "init.el")
   wl-folders-file (path-join *user-wanderlust-data-directory* "folders")
   wl-address-file (path-join *user-wanderlust-data-directory* "addresses")
   ;; Put temporary files in cache directories.
   wl-temporary-file-directory *user-wanderlust-cache-directory*
   ssl-certificate-directory (path-join *user-cache-directory* "certs")
   ;; Mark sent mails as read.
   wl-fcc-force-as-read t
   ;; Check for mail when idle.
   wl-biff-check-interval 180
   wl-biff-use-idle-timer t
   ;; Let SMTP server handle Message-ID.
   wl-insert-message-id nil
   ;; Message window size.
   wl-message-window-size '(1 . 3)
   ;; Quit without asking.
   wl-interactive-exit nil

   ;;; (Folders) ;;;
   ;; Show folders in a pane to the left.
   wl-stay-folder-window t
   wl-folder-window-width 30
   ;; Asynchronously update folders.
   wl-folder-check-async t

   ;;; (Summary) ;;;
   ;; Set verbose summary.
   wl-summary-width nil
   wl-summary-line-format "%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"
   ;; UTF-8 guides.
   wl-thread-have-younger-brother-str "├──►"
   wl-thread-youngest-child-str       "╰──►"
   wl-thread-vertical-str             "|"
   wl-thread-horizontal-str           "►"
   wl-thread-horizontal-str           "►"
   wl-thread-space-str                " "

   ;;; (Messages) ;;;
   ;; Field lists.
   wl-message-ignored-field-list '("^.*")
   wl-message-visible-field-list '("^\\(To\\|Cc\\):" "^Subject:"
                                   "^\\(From\\|Reply-To\\):" "^Organization:"
                                   "^X-Attribution:" "^\\(Posted\\|Date\\):"
                                   "^\\(Posted\\|Date\\):"
                                   "^\\(User-Agent\\|X-Mailer\\):")
   wl-message-sort-field-list    '("^From:" "^Organization:" "^X-Attribution:"
                                   "^Subject:" "^Date:" "^To:" "^Cc:")

   ;;; (Drafts) ;;;
   ;; Raise a new frame when creating a draft.
   wl-draft-use-frame t
   ;; Automatically save drafts every two minutes.
   wl-auto-save-drafts-interval 120.0
   ;; Sane forward tag.
   wl-forward-subject-prefix "Fwd: "
   ;; Automatically select the correct template based on folder.
   wl-draft-config-matchone t)

  (with-feature 'fullframe
    (fullframe wl wl-exit nil))

  (setq-default
   ;; Show mail status in mode line.
   global-mode-string (cons '(wl-modeline-biff-status
                              wl-modeline-biff-state-on
                              wl-modeline-biff-state-off) global-mode-string))

  ;; Pick email account template when opening a draft.
  (add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)
  ;; Don't apply email account template when sending draft, otherwise switching
  ;; templates won't work.
  (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)

  (add-hook 'wl-init-hook 'user/wl-init-hook)
  (add-hook 'wl-folder-mode-hook 'user/wl-folder-mode-hook)
  (add-hook 'wl-summary-mode-hook 'user/wl-summary-mode-hook)
  (add-hook 'wl-message-redisplay-hook 'user/wl-message-redisplay-hook)
  (add-hook 'wl-message-buffer-created-hook
            'user/wl-message-buffer-created-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :email 'wl))


(defun user/wl-init ()
  "Initialize wanderlust."
  ;; Create data and cache stores.
  (make-directory *user-wanderlust-data-directory* t)
  (make-directory *user-wanderlust-cache-directory* t)

  (require-package '(:name wanderlust :after (user/wanderlust-init)))
  (require-package '(:name bbdbv3-wl)))

(user/wl-init)


(provide 'apps/wanderlust)
;;; wanderlust.el ends here
