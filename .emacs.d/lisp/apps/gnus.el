;;; gnus.el --- Gnus mail and news management system
;;; Commentary:
;;; Code:

(defconst *user-gnus-data-directory*
  (path-join *user-data-directory* "gnus")
  "Path to user's Gnus data store.")

(defconst *user-gnus-cache-directory*
  (path-join *user-cache-directory* "gnus")
  "Path to user's Gnus cache store.")


(defun user/gnus-group-mode-hook ()
  "Gnus group mode hook."
  (setq header-line-format "    Ticked    New     Unread   Group")
  (gnus-topic-mode t)
  (hl-line-mode t)

  ;;; (Bindings) ;;;
  (define-key gnus-group-mode-map (kbd "v s") 'user/gnus-mailsync))


(defun user/gnus-summary-mode-hook ()
  "Gnus summary mode hook."
  (hl-line-mode t))


(defun user/gnus-agent-plugged-hook ()
  "Gnus agent plugged mode hook."
  (setq
   ;; Stop queueing email.
   smtpmail-queue-mail nil))


(defun user/gnus-agent-unplugged-hook ()
  "Gnus agent unplugged mode hook."
  (setq
   ;; Start queueing email.
   smtpmail-queue-mail t))


(defun user/gnus-message-sent-hook ()
  "Message sent hook for Gnus."
  ;; Increase score for followups to a sent article.
  (gnus-score-followup-article)
  (gnus-score-followup-thread))


(defun user/gnus-startup-hook ()
  "Gnus startup hook."
  ;; Enable BBDB.
  (bbdb-initialize 'gnus)
  (when (feature-p 'google-contacts)
    ;; Google Contacts for Gnus.
    (require 'google-contacts-gnus))
  ;; Enable S/MIME via EasyPG.
  (epa-file-enable)
  (with-feature 'gnus-dired
    ;; Attach files using dired.
    (turn-on-gnus-dired-mode)))


(defun user/gnus-mailsync ()
  "Sync tags, empty the queue and download all mail."
  (interactive)
  (gnus-agent-while-plugged
    (gnus-agent-synchronize-flags)
    (gnus-group-get-new-news)
    (gnus-agent-fetch-session)))


(defun user/gnus-set-gmail-user (fullname username)
  "Configure Gnus to use \"FULLNAME\" <USERNAME@gmail.com>."
  (let ((email-address (concat username "@gmail.com")))
    (require 'gnus-msg)
    (add-to-list
     'gnus-posting-styles
     `(,email-address
       (name ,fullname)
       (address ,email-address)))

    (add-to-list
     'gnus-secondary-select-methods
     `(nnimap ,email-address
              (nnimap-address "imap.gmail.com")
              (nnimap-server-port 993)
              (nnimap-stream tls)
              (nnimap-list-pattern ("INBOX" "*"))
              (nnimap-expunge-on-close always)
              (gnus-check-new-newsgroups nil)
              (gnus-ignored-newsgroups
               "^to\\.\\|^[0-9.\t]+\\( \\|$\\)\\|^[\”]\”[#’()]")))

    (add-to-list
     'gnus-parameters
     `(,(concat "nnimap " email-address ":[Gmail]/.*")
       (display . all)
       (posting-style
        (name ,fullname)
        (address ,email-address)
        (gcc ,(concat "nnimap+" email-address ":%[Gmail]/Sent")))
       (expiry-wait . never)))

    (add-to-list
     'gnus-message-archive-group
     `(,(concat email-address ".*")
       ,(concat "nnimap+" email-address ":%[Gmail]/Sent")))

    (user/smtpmail-set-gmail-user fullname username)))


(defun user/gnus-score-init ()
  "Initialize Gnus scoring system."
  (setq-default
   ;; Gnus article scoring entries.
   gnus-home-score-file (path-join *user-gnus-data-directory* "score")
   ;; Number of days to keep score.
   gnus-score-expiry-days 60
   ;; Use adaptive scoring.
   gnus-use-adaptive-scoring '(word line)
   ;; Adaptive score list.
   gnus-default-adaptive-score-alist
   '((gnus-unread-mark)
     (gnus-ticked-mark (from 4))
     (gnus-dormant-mark (from 5))
     (gnus-saved-mark (from 20) (subject 5))
     (gnus-del-mark (from -2) (subject -5))
     (gnus-read-mark (from 2) (subject 1))
     (gnus-killed-mark (from -1) (subject -3)))
   ;; Decay score over time.
   gnus-decay-scores t
   ;; Score decay rate.
   gnus-score-decay-constant 1
   gnus-score-decay-scale 0.03
   ;; Workaround for GMail folder names.
   nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_))))


(defun user/gnus-mime-init ()
  "Initialize Gnus MIME."
  (setq-default
   ;; Default location for downloaded attachments.
   mm-default-directory (path-join *user-home-directory* "Downloads")
   ;; Buttonized MIME types.
   gnus-buttonized-mime-types
   '("multipart/alternative"
     "multipart/encrypted"
     "multipart/signed")
   ;; Prefer plaintext emails.
   mm-discouraged-alternatives '("text/html" "text/richtext")
   ;; Display text/html only mails in Emacs.
   mm-automatic-display
   '("text/plain" "text/enriched" "text/richtext"
     "image/.*" "message/delivery-status" "multipart/.*" "message/rfc822"
     "text/x-patch" "application/pgp-signature" "application/emacs-lisp")
   ;; HTML rendering method.
   mm-text-html-renderer
   (cond ((feature-p 'shr) 'mm-shr)
         ((executable-find "w3m") 'w3m))
   ;; Use EasyPG for signing and encryption of emails.
   mml-smime-use 'epg
   ;; Always decrypt emails.
   mm-decrypt-option 'always
   ;; Always verify signed emails.
   mm-verify-option 'always
   ;; Set some sane encoding types.
   mm-content-transfer-encoding-defaults
   '(;; Use 8-bit encoding for readable files.
     ("text/x-patch" 8bit)
     ("text/.*" 8bit)
     ("message/rfc822" 8bit)
     ("application/emacs-lisp" 8bit)
     ("application/x-emacs-lisp" 8bit)
     ("application/x-patch" 8bit)
     (".*" base64))
   ;; Avoid spaces in filenames when saving attachments.
   mm-file-name-rewrite-functions
   '(mm-file-name-trim-whitespace
     mm-file-name-collapse-whitespace
     mm-file-name-replace-whitespace))

  (let ((ca-directory "/etc/ssl/certs"))
    (when (file-exists-p ca-directory)
      (setq-default smime-CA-directory ca-directory)))

  (let ((certificate-directory (path-join *user-home-directory* ".ssl")))
    (when (file-exists-p certificate-directory)
      (setq-default
       ;; User certificate store.
       smime-certificate-directory certificate-directory)))

  (when (eq default-terminal-coding-system 'utf-8)
    (setq-default mm-coding-system-priorities '(utf-8)))

  (when (display-graphic-p)
    ;; Display images inline.
    (setq-default
     mm-inline-text-html-with-images t
     mm-inline-large-images t)

    (after-load 'mm-decode
      (add-to-list 'mm-attachment-override-types "image/.*"))))


(defun user/gnus-groups-init ()
  "Initialize Gnus group mode."
  (setq-default
   ;; Groups format.
   gnus-group-line-format
   (concat "%M %1(%1{%6i %}%)%{ %}%2(%2{%7U %}%)%{ %}%3(%3{%7y %}%)%{%* %}"
           "%4(%B%-45G%)\n")
   ;; Group sort method.
   gnus-group-sort-function
   '(gnus-group-sort-by-score
     gnus-group-sort-by-unread
     gnus-group-sort-by-alphabet
     gnus-group-sort-by-level))

  (when (eq default-terminal-coding-system 'utf-8)
    (setq-default
     ;; Mark characters.
     gnus-score-over-mark  ?↑
     gnus-score-below-mark ?↓
     gnus-ticked-mark      ?⚑
     gnus-dormant-mark     ?⚐
     gnus-expirable-mark   ?♻
     gnus-read-mark        ?✓
     gnus-del-mark         ?✗
     gnus-killed-mark      ?☠
     gnus-replied-mark     ?⟲
     gnus-forwarded-mark   ?⤳
     gnus-cached-mark      ?☍
     gnus-recent-mark      ?★
     gnus-unseen-mark      ?✩
     gnus-unread-mark      ?✉))

  ;; Use smtpmail queue instead of Gnus queue.
  (defadvice gnus-group-send-queue (after smtp-flush-queue activate)
    "Empty the smtpmail queue after emptying the gnus send queue."
    (smtpmail-send-queued-mail))

  ;;; (Hooks) ;;;
  (add-hook 'gnus-group-mode-hook 'user/gnus-group-mode-hook))


(defun user/gnus-summary-init ()
  "Initialize Gnus summary."
  (setq-default
   ;; Summary format.
   gnus-summary-line-format
   (concat "%z%U%R %~(max-right 17)~(pad-right 17)&user-date;  "
           "%~(max-right 20)~(pad-right 20)f %B%s\n")
   ;; Thread construction method.
   gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
   ;; Collapse threads by default.
   gnus-thread-hide-subtree t
   ;; Thread sort method.
   gnus-thread-sort-functions
   '((not gnus-thread-sort-by-total-score)
     (not gnus-thread-sort-by-most-recent-number)
     (not gnus-thread-sort-by-most-recent-date))
   gnus-subthread-sort-functions
   '(gnus-thread-sort-by-number
     gnus-thread-sort-by-date)
   gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date
   ;; Don't automatically open next message when reaching end.
   gnus-summary-stop-at-end-of-message t
   ;; Simplify message subjects.
   gnus-simplify-subject-functions
   '(gnus-simplify-subject-re
     gnus-simplify-whitespace))

  (if (eq default-terminal-coding-system 'utf-8)
      (setq-default
       ;; Summary line characters.
       gnus-summary-to-prefix        "→"
       gnus-summary-newsgroup-prefix "⇶"
       ;; Summary thread guides.
       gnus-sum-thread-tree-indent          "  "
       gnus-sum-thread-tree-single-indent   "◎ "
       gnus-sum-thread-tree-root            "● "
       gnus-sum-thread-tree-false-root      "◌ "
       gnus-sum-thread-tree-vertical        "│ "
       gnus-sum-thread-tree-leaf-with-other "├─▶ "
       gnus-sum-thread-tree-single-leaf     "└─▶ ")
    (setq-default
     gnus-sum-thread-tree-indent          " "
     gnus-sum-thread-tree-single-indent   " "
     gnus-sum-thread-tree-root            ""
     gnus-sum-thread-tree-false-root      ""
     gnus-sum-thread-tree-vertical        "|"
     gnus-sum-thread-tree-leaf-with-other "+-> "
     gnus-sum-thread-tree-single-leaf     "\\-> "))

  ;;; (Hooks) ;;;
  (add-hook 'gnus-summary-mode-hook 'user/gnus-summary-mode-hook)

  ;;; (Bindings) ;;;
  (after-load 'gnus
    (define-key gnus-summary-mode-map (kbd "[")
      (lambda () (interactive) (scroll-other-window -1)))
    (define-key gnus-summary-mode-map (kbd "]")
      (lambda () (interactive) (scroll-other-window 1)))))


(defun user/gnus-agent-init ()
  "Initialize Gnus agent."
  (setq-default
   ;; Automatically go online when plugged in.
   gnus-agent-go-online t)

  ;; Set up smtpmail queue based on Gnus queue state.
  (defadvice gnus (after gnus-queue-off activate)
    "Turn off and flush the smtpmail queue when starting a plugged gnus."
    (setq smtpmail-queue-mail nil)
    (when (file-exists-p (path-join smtpmail-queue-dir "index"))
      (smtpmail-send-queued-mail)))
  (defadvice gnus-unplugged (after gnus-queue-on activate)
    "Turn on the smtpmail queue when starting an unplugged gnus."
    (setq smtpmail-queue-mail t)))


(defun user/gnus-init ()
  "Initialize Gnus."
  (setq-default
   ;; Make Gnus the default mail reader.
   read-mail-command 'gnus
   ;; Gnus cache store.
   gnus-use-cache t
   gnus-cache-directory *user-gnus-cache-directory*
   ;; Cache all types of articles and never remove them.
   gnus-cache-enter-articles '(ticked dormant read unread)
   gnus-cache-remove-articles nil
   ;; Types of groups to cache.
   gnus-cacheable-groups "^nnimap"
   ;; Default method should be mail spooler.
   gnus-select-method '(nnml "")
   ;; Gnus data store.
   gnus-directory *user-gnus-data-directory*
   message-directory (path-join *user-gnus-data-directory* "mail")
   nnfolder-directory (path-join *user-gnus-data-directory* "mail" "archive")
   nnfolder-active-file (path-join *user-gnus-data-directory*
                                   "mail" "archive" "active")
   smtpmail-queue-dir (path-join *user-gnus-data-directory* "mail" "queued-mail")
   gnus-article-save-directory (path-join *user-gnus-data-directory* "articles")
   ;; Enable asynchronous operations.
   gnus-asynchronous t
   ;; Asynchronous header prefetch.
   gnus-use-header-prefetch t
   ;; Default method.
   gnus-select-method '(nnml "mail")
   ;; Disable newsreader features.
   gnus-save-newsrc-file nil
   gnus-read-newsrc-file nil
   gnus-always-read-dribble-file t
   ;; Headers visible by default.
   gnus-visible-headers
   (concat
    "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:"
    "\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:"
    "\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:"
    "\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:"
    "\\|^X-Mailer:\\|^X-Newsreader:")
   ;; Sort order.
   gnus-sorted-header-list
   '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:"
     "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:"
     "^X-Newsreader:")
   ;; Date format.
   gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
   ;; Suppress startup message.
   gnus-inhibit-startup-message t
   ;; Don't require confirmation before downloading folders.
   gnus-large-newsgroup nil
   ;; Mark sent messages as read.
   gnus-gcc-mark-as-read t
   ;; Keep password cache longer.
   password-cache-expiry 3600
   ;; Don't require confirmation on exit.
   gnus-interactive-exit nil
   ;; Always attempt to build complete threads.
   gnus-fetch-old-headers t)

  (setq-default
   ;; Archive using nnfolder.
   gnus-message-archive-method
   `(nnfolder "archive"
              (nnfolder-directory ,(path-join message-directory "archive"))
              (nnfolder-active-file ,(path-join message-directory
                                                "archive" "active"))
              (nnfolder-get-new-mail nil)
              (nnfolder-inhibit-expiry t)))

  (when (display-graphic-p)
    (setq-default
     ;; Properties for Face and X-Face attributes.
     gnus-face-properties-alist
     '((pbm . (:face gnus-x-face :ascent center))
       (png . (:ascent center)))
     ;; Render images in HTML mail.
     gnus-blocked-images nil
     ;; Enable gravatars.
     gnus-treat-from-gravatar 'head
     gnus-treat-mail-gravatar 'head
     gnus-gravatar-properties '(:ascent center)))

  ;; Protect data and cache stores.
  (make-directory *user-gnus-data-directory* t)
  (set-file-modes *user-gnus-data-directory* #o0700)
  (make-directory *user-gnus-cache-directory* t)
  (set-file-modes *user-gnus-cache-directory* #o0700)

  ;; Gnus modes.
  (user/gnus-agent-init)
  (user/gnus-mime-init)
  (user/gnus-summary-init)
  (user/gnus-groups-init)
  (user/gnus-score-init)

  ;; Hooks
  (add-hook 'gnus-startup-hook 'user/gnus-startup-hook)
  (add-hook 'message-sent-hook 'user/gnus-message-sent-hook))

(require-package '(:name gnus :after (user/gnus-init)))
(with-executable 'gpgsm
  (require-package '(:name jl-smime)))
(when (display-graphic-p)
  (require-package '(:name gnus-gravatar)))


(provide 'apps/gnus)
;;; gnus.el ends here
