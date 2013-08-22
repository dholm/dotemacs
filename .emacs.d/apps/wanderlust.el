;;; wanderlust.el --- wanderlust mail and news management system
;;; Commentary:
;;; Code:

(defconst *user-wanderlust-data-directory*
  (path-join *user-data-directory* "wanderlust")
  "Path to user's Wanderlust data store.")

(defconst *user-wanderlust-cache-directory*
  (path-join *user-cache-directory* "wanderlust")
  "Path to user's Wanderlust cache store.")

(defun user/wanderlust-init ()
  "Initialize Wanderlust."
  (setq-default
   ;; Put configuration into wanderlust data directory
   wl-init-file (path-join *user-wanderlust-data-directory* "init.el")
   wl-folders-file (path-join *user-wanderlust-data-directory* "folders")
   wl-address-file (path-join *user-wanderlust-data-directory* "addresses")
   ;; Put message database in data directory
   elmo-msgdb-directory (path-join *user-wanderlust-data-directory* "elmo")
   ;; Put temporary files in cache directories
   wl-temporary-file-directory *user-wanderlust-cache-directory*
   elmo-cache-directory (path-join *user-wanderlust-cache-directory* "elmo")
   ssl-certificate-directory (path-join *user-cache-directory* "certs")
   ;; Show folders in a pane to the left
   wl-stay-folder-window t
   wl-folder-window-width 30
   ;; Automatically save drafts every two minutes
   wl-auto-save-drafts-interval 120.0
   ;; Mark sent mails as read
   wl-fcc-force-as-read t
   ;; Set verbose summary
   wl-summary-width 150
   wl-summary-line-format "%n%T%P %D/%M (%W) %h:%m %t%[%25(%c %f%) %] %s")

  ;; Set up wanderlust as the default mail user agent
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
    (solarized-with-values
      (eval
       `(custom-theme-set-faces
         'solarized
         '(wl-highlight-folder-few-face ((t (:foreground ,red))))
         '(wl-highlight-folder-many-face ((t (:foreground ,red))))
         '(wl-highlight-folder-path-face ((t (:foreground ,orange))))
         '(wl-highlight-folder-unread-face ((t (:foreground ,blue))))
         '(wl-highlight-folder-zero-face ((t (:foreground ,solarized-fg))))
         '(wl-highlight-folder-unknown-face ((t (:foreground ,blue))))
         '(wl-highlight-message-citation-header ((t (:foreground ,red))))
         '(wl-highlight-message-cited-text-1 ((t (:foreground ,red))))
         '(wl-highlight-message-cited-text-2 ((t (:foreground ,green))))
         '(wl-highlight-message-cited-text-3 ((t (:foreground ,blue))))
         '(wl-highlight-message-cited-text-4 ((t (:foreground ,blue))))
         '(wl-highlight-message-header-contents-face ((t (:foreground ,green))))
         '(wl-highlight-message-headers-face ((t (:foreground ,red))))
         '(wl-highlight-message-important-header-contents ((t (:foreground ,green))))
         '(wl-highlight-message-header-contents ((t (:foreground ,green))))
         '(wl-highlight-message-important-header-contents2 ((t (:foreground ,green))))
         '(wl-highlight-message-signature ((t (:foreground ,green))))
         '(wl-highlight-message-unimportant-header-contents ((t (:foreground ,solarized-fg))))
         '(wl-highlight-summary-answered-face ((t (:foreground ,blue))))
         '(wl-highlight-summary-disposed-face ((t (:foreground ,solarized-fg
                                                               :slant italic))))
         '(wl-highlight-summary-new-face ((t (:foreground ,blue))))
         '(wl-highlight-summary-normal-face ((t (:foreground ,solarized-fg))))
         '(wl-highlight-summary-thread-top-face ((t (:foreground ,yellow))))
         '(wl-highlight-thread-indent-face ((t (:foreground ,magenta))))
         '(wl-highlight-summary-refiled-face ((t (:foreground ,solarized-fg))))
         '(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))))))

  ;;; (Bindings) ;;;
  (define-key user/utilities-map (kbd "m") 'wl))


(defun user/wanderlust-set-gmail-user (fullname username)
  "Configure Wanderlust to use \"FULLNAME\" <USERNAME@gmail.com>."
  (make-directory *user-wanderlust-data-directory* t)
  (let ((email-address (concat username "@gmail.com")))
    (unless (file-exists-p wl-folders-file)
      (user/wanderlust-create-folders-gmail username wl-folders-file))
    (setq-default
     ;; Setup mail-from
     wl-from (concat fullname " <" email-address ">")
     ;; Sane forward tag
     wl-forward-subject-prefix "Fwd: ")

    ;; IMAP
    (setq-default
     elmo-imap4-default-server "imap.gmail.com"
     elmo-imap4-default-user email-address
     elmo-imap4-default-authenticate-type 'clear
     elmo-imap4-default-port '993
     elmo-imap4-default-stream-type 'ssl)

    (setq-default
     elmo-imap4-use-modified-utf7 t)

    ;; SMTP
    (setq-default
     wl-smtp-connection-type 'starttls
     wl-smtp-posting-port 587
     wl-smtp-authenticate-type "plain"
     wl-smtp-posting-user username
     wl-smtp-posting-server "smtp.gmail.com"
     wl-local-domain "gmail.com")

    ;; Folders
    (setq-default
     wl-folder-check-async t
     wl-default-folder "%inbox"
     wl-default-spec "%"
     wl-draft-folder "%[Gmail]/Drafts"
     wl-spam-folder "%[Gmail]/Spam"
     wl-trash-folder "%[Gmail]/Trash")

    (setq-default
     wl-insert-message-id nil)))


(defun user/wanderlust-create-folders-gmail (username folders-file)
  "Write GMail folders for USERNAME@gmail.com to FOLDERS-FILE."
  (let ((folders-template "# -*- conf-unix -*-
+trash  \"Trash\"
+draft  \"Drafts\"

Gmail{
    %inbox:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Inbox\"
    Labels{
        %[Gmail]/Important:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Important\"
        %[Gmail]/Drafts:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Drafts\"
        %[Gmail]/All Mail:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"All Mail\"
        %[Gmail]/Sent Mail:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Sent Mail\"
        %[Gmail]/Starred:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Starred\"
        %[Gmail]/Spam:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Spam\"
        %[Gmail]/Trash:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Trash\"
    }
    %Org-Mode:\"USERNAME@gmail.com\"/clear@imap.gmail.com:993! \"Org-Mode\"
}
"))
    (with-temp-buffer
      (insert
       (replace-regexp-in-string "USERNAME" username folders-template t))
      (when (file-writable-p folders-file)
       (write-region (point-min) (point-max) folders-file)))))


(require-package '(:name wanderlust :after (user/wanderlust-init)))


(provide 'apps/wanderlust)
;;; wanderlust.el ends here
