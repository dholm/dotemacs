;;; gnus.el --- Gnus mail and news management system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-gnus-data-directory*
  (path-join *user-data-directory* "gnus")
  "Path to user's Gnus data store.")

(defconst *user-gnus-cache-directory*
  (path-join *user-cache-directory* "gnus")
  "Path to user's Gnus cache store.")


(defun user--gnus-group-mode-hook ()
  "Gnus group mode hook."
  (setq header-line-format "    Ticked    New     Unread   Group")
  (gnus-topic-mode t)
  (hl-line-mode t)

  ;;; (Bindings) ;;;
  (define-key gnus-group-mode-map (kbd "v s") 'user/gnus-mailsync))


(defun user--gnus-summary-mode-hook ()
  "Gnus summary mode hook."
  (hl-line-mode t))


(defun user--gnus-article-prepare-hook ()
  "Gnus article display hook."
  (when (featurep 'bbdb)
    (user/bbdb-display-record)))


(defun user--gnus-agent-plugged-hook ()
  "Gnus agent plugged mode hook."
  (setq
   ;; Stop queueing email.
   smtpmail-queue-mail nil))


(defun user--gnus-agent-unplugged-hook ()
  "Gnus agent unplugged mode hook."
  (setq
   ;; Start queueing email.
   smtpmail-queue-mail t))


(defun user--gnus-message-sent-hook ()
  "Message sent hook for Gnus."
  ;; Increase score for followups to a sent article.
  (gnus-score-followup-article)
  (gnus-score-followup-thread))


(defun user--gnus-startup-hook ()
  "Gnus startup hook."
  ;; Enable BBDB.
  (bbdb-initialize 'gnus 'message)
  ;; Automatically add seen subjects to notes.
  (add-to-list 'bbdb-auto-notes-rules
               '("Subject" (".*" subjects user/gnus-bbdb-subject-canonicalize
                            nil)))
  ;; Automatically create entries in BBDB.
  (bbdb-mua-auto-update-init 'gnus 'message)
  (setq
   bbdb-mua-update-interactive-p '(query . create)
   bbdb-mua-auto-update-p 'create)

  (when (feature-p 'google-contacts)
    ;; Google Contacts for Gnus.
    (require 'google-contacts-gnus))

  ;; Enable S/MIME via EasyPG.
  (epa-file-enable)
  (when (feature-p 'jl-smime)
    ;; S/MIME LDAP support.
    (require 'ldap)
    (load "jl-smime"))

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
               "^to\\.\\|^[0-9.\t]+\\( \\|$\\)\\|^[\”]\”[#’()]")
              (nnir-search-engine imap)))

    (add-to-list
     'gnus-parameters
     `(,(concat email-address ".*")
       (display . all)
       (posting-style
        (name ,fullname)
        (address ,email-address)
        (gcc ,(concat "nnimap+" email-address ":[Gmail]/Sent")))
       (expiry-wait . never)))

    (add-to-list
     'gnus-message-archive-method
     `(nnimap ,email-address
              (nnfolder-directory ,(concat "nnimap+" email-address ":\"Sent Items\""))
              (nnfolder-get-new-mail t)
              (nnfolder-inhibit-expiry t)))

    (add-to-list 'nnimap-inbox (concat "nnimap+" email-address ":INBOX"))

    (user/smtpmail-set-gmail-user fullname username)))


(defun user/gnus-bbdb-subject-canonicalize (subject)
  "Canonicalize SUBJECT."
  (let ((newsubject (message-strip-subject-trailing-was
                     (message-strip-subject-encoded-words
                      (message-strip-subject-re
                       (mail-decode-encoded-word-string subject))))))
    newsubject))


(defun user--gnus-agent-config ()
  "Initialize Gnus agent."
  (validate-setq
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


(use-package gnus
  :commands gnus
  :init
  ;; Create data and cache stores.
  (let ((article-dir (path-join *user-gnus-data-directory* "articles")))
    (make-directory article-dir t)
    (set-file-modes article-dir #o0700))

  ;; Protect data and cache stores.
  (make-directory *user-gnus-data-directory* t)
  (set-file-modes *user-gnus-data-directory* #o0700)
  (make-directory *user-gnus-cache-directory* t)
  (set-file-modes *user-gnus-cache-directory* #o0700)

  ;; Hooks
  (add-hook 'gnus-startup-hook 'user--gnus-startup-hook)
  (add-hook 'message-sent-hook 'user--gnus-message-sent-hook)
  :config
  (validate-setq
   ;; Make Gnus the default mail reader.
   read-mail-command 'gnus
   ;; Gnus cache store.
   gnus-use-cache t
   ;; Default method should be mail spooler.
   gnus-select-method '(nnml "")
   ;; Gnus data store.
   gnus-directory *user-gnus-data-directory*
   gnus-article-save-directory (path-join *user-gnus-data-directory* "articles")
   ;; Default method.
   gnus-select-method '(nnml "mail")
   ;; Suppress startup message.
   gnus-inhibit-startup-message t
   ;; Don't require confirmation before downloading folders.
   gnus-large-newsgroup nil
   ;; Don't require confirmation on exit.
   gnus-interactive-exit nil
   ;;; (Summary) ;;;
   ;; Summary format.
   gnus-summary-line-format
   (concat "%z%U%R %~(max-right 17)~(pad-right 17)&user-date;  "
           "%~(max-right 20)~(pad-right 20)f %B%s\n")
   ;;; (Scoring) ;;;
   ;; Use adaptive scoring.
   gnus-use-adaptive-scoring '(word line))

  (after-load 'message
    (validate-setq
     message-directory (path-join *user-gnus-data-directory* "mail")))

  (after-load 'smtpmail
    (validate-setq
     smtpmail-queue-dir (path-join *user-gnus-data-directory* "mail" "queued-mail")))

  (validate-setq
   ;; Archive using nnfolder.
   gnus-message-archive-method
   `(nnfolder "archive"
              (nnfolder-directory ,(path-join message-directory "archive"))
              (nnfolder-active-file ,(path-join message-directory
                                                "archive" "active"))
              (nnfolder-get-new-mail nil)
              (nnfolder-inhibit-expiry t)))

  (with-feature 'fullframe
    (fullframe gnus gnus-group-exit nil))

  (use-package gnus-start
    :ensure gnus
    :config
    (validate-setq
     ;; Disable newsreader features.
     gnus-save-newsrc-file nil
     gnus-read-newsrc-file nil
     gnus-always-read-dribble-file t))

  (use-package gnus-agent
    :ensure gnus
    :config (user--gnus-agent-config))

  (use-package gnus-art
    :ensure gnus
    :config
    (validate-setq
     ;; Headers visible by default.
     gnus-visible-headers
     (concat
      "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:"
      "\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:"
      "\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:"
      "\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:"
      "\\|^Message-ID:"
      "\\|^X-Mailer:\\|^X-Newsreader:\\|^List-Id:")
     ;; Sort order.
     gnus-sorted-header-list
     '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^List-Id:"
       "^Followup-To:" "^To:" "^Cc:" "^Date:" "^User-Agent:" "^X-Mailer:"
       "^X-Newsreader:")
     ;; Buttonized MIME types.
     gnus-buttonized-mime-types
     '("multipart/alternative"
       "multipart/encrypted"
       "multipart/signed"))

    (when (display-graphic-p)
      (validate-setq
       ;; Properties for Face and X-Face attributes.
       gnus-face-properties-alist
       '((pbm . (:face gnus-x-face :ascent center))
         (png . (:ascent center)))
       ;; Render images in HTML mail.
       gnus-blocked-images nil
       ;; Enable gravatars.
       gnus-treat-from-gravatar 'head
       gnus-treat-mail-gravatar 'head))

    ;;; (Hooks) ;;;
    (add-hook 'gnus-article-prepare-hook 'user--gnus-article-prepare-hook))

  (use-package gnus-group
    :ensure gnus
    :config
    (validate-setq
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

    ;; Use smtpmail queue instead of Gnus queue.
    (defadvice gnus-group-send-queue (after smtp-flush-queue activate)
      "Empty the smtpmail queue after emptying the gnus send queue."
      (smtpmail-send-queued-mail))

    ;;; (Hooks) ;;;
    (add-hook 'gnus-group-mode-hook 'user--gnus-group-mode-hook))

  (use-package gnus-sum
    :ensure gnus
    :config
    (validate-setq
     ;; Date format.
     gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
     ;; Always attempt to build complete threads.
     gnus-fetch-old-headers t
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
        (validate-setq
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
         gnus-sum-thread-tree-single-leaf     "└─▶ "
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
         gnus-unread-mark      ?✉)
      (validate-setq
       gnus-sum-thread-tree-indent          " "
       gnus-sum-thread-tree-single-indent   " "
       gnus-sum-thread-tree-root            ""
       gnus-sum-thread-tree-false-root      ""
       gnus-sum-thread-tree-vertical        "|"
       gnus-sum-thread-tree-leaf-with-other "+-> "
       gnus-sum-thread-tree-single-leaf     "\\-> "))

    ;;; (Hooks) ;;;
    (add-hook 'gnus-summary-mode-hook 'user--gnus-summary-mode-hook)

    ;;; (Bindings) ;;;
    (after-load 'gnus
      (define-key gnus-summary-mode-map (kbd "[")
        (lambda () (interactive) (scroll-other-window -1)))
      (define-key gnus-summary-mode-map (kbd "]")
        (lambda () (interactive) (scroll-other-window 1)))))

  (use-package gnus-msg
    :ensure gnus
    :config
    (validate-setq
     ;; Mark sent messages as read.
     gnus-gcc-mark-as-read t))

  (use-package gnus-async
    :ensure gnus
    :config
    (validate-setq
     ;; Enable asynchronous operations.
     gnus-asynchronous t
     ;; Asynchronous header prefetch.
     gnus-use-header-prefetch t))

  (use-package gnus-cache
    :ensure gnus
    :config
    (validate-setq
     ;; Gnus cache store.
     gnus-cache-directory *user-gnus-cache-directory*
     gnus-cache-active-file (path-join *user-gnus-cache-directory* "active")
     ;; Cache all types of articles and never remove them.
     gnus-cache-enter-articles '(ticked dormant read unread)
     gnus-cache-remove-articles nil
     ;; Types of groups to cache.
     gnus-cacheable-groups "^nnimap"))

  (use-package gnus-score
    :ensure gnus
    :config
    (validate-setq
     ;; Gnus article scoring entries.
     gnus-home-score-file (path-join *user-gnus-data-directory* "score")
     ;; Number of days to keep score.
     gnus-score-expiry-days 60
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
     gnus-score-decay-scale 0.03))

  (use-package gnus-alias)
  (use-package gnus-summary-ext)
  (use-package gnus-x-gm-raw)
  (when (feature-p 'gnus-x-gm-raw)
    ;; GMail search.
    (require 'gnus-x-gm-raw))

  (use-package gnus-gravatar
    :if window-system
    :ensure nil
    :config
    (validate-setq
     gnus-gravatar-properties '(:ascent center)))

  (use-package nnheader
    :ensure nil
    :config
    (validate-setq
     ;; Workaround for scoring GMail folder names.
     nnheader-file-name-translation-alist '((?[ . ?_) (?] . ?_))))

  (use-package nnfolder
    :ensure nil
    :init
    (make-directory (path-join *user-gnus-data-directory* "mail"
                               "archive" "active") t)
    (set-file-modes (path-join *user-gnus-data-directory*
                               "mail" "archive") #o0700)
    (set-file-modes (path-join *user-gnus-data-directory*
                               "mail" "archive" "active") #o0700)
    :config
    (validate-setq
     ;; nnfolder data store.
     nnfolder-directory (path-join *user-gnus-data-directory* "mail" "archive")
     nnfolder-active-file (path-join *user-gnus-data-directory*
                                     "mail" "archive" "active")))

  (use-package nnmail
    :ensure nil
    :config
    (validate-setq
     ;; Stop splitting at first matching rule.
     nnmail-crosspost nil
     ;; Use the same function to split nnmail and nnimap.
     nnmail-split-methods 'nnmail-split-fancy
     ;; Warn about duplicate mail.
     nnmail-treat-duplicates 'warn
     ;; Default split rules.
     nnmail-split-fancy
     '(|
       ;; Split followups based on parent's message id.
       (: nnmail-split-fancy-with-parent)))

    (after-load 'nnmail
      (add-many-to-list
       ;; Add keywords for matching articles while splitting.
       'nnmail-split-abbrev-alist
       ;; Mailing lists.
       '(list . "list-id")
       ;; Subject.
       '(subject . "subject"))))

  (use-package nnimap
    :ensure nil
    :config
    (validate-setq
     ;; Use the same function to split nnmail and nnimap.
     nnimap-split-methods 'nnmail-split-fancy
     ;; Initialize list of mailboxes where splitting should occur.
     nnimap-inbox nil
     ;; Only download textual parts of articles by default.
     nnimap-fetch-partial-articles "text/"
     ;; Always download entire article when splitting.
     nnimap-split-download-body-default t))

  (use-package mm-decode
    :ensure nil
    :config
    (validate-setq
     ;; Default location for downloaded attachments.
     mm-default-directory (path-join *user-home-directory* "Downloads")
     ;; Always decrypt emails.
     mm-decrypt-option 'always
     ;; Always verify signed emails.
     mm-verify-option 'always
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
           ((feature-p 'emacs-w3m) 'w3m)))
    ;; Don't use validate-setq due to :inline not being supported.
    (setq
     ;; Avoid spaces in filenames when saving attachments.
     mm-file-name-rewrite-functions
     '(mm-file-name-trim-whitespace
       mm-file-name-collapse-whitespace
       mm-file-name-replace-whitespace))

    (when (display-graphic-p)
      ;; Display images inline.
      (validate-setq
       mm-inline-large-images t)

      (add-to-list 'mm-attachment-override-types "image/.*")))

  (use-package mm-encode
    :ensure nil
    :config
    (validate-setq
     ;; Set some sane encoding types.
     mm-content-transfer-encoding-defaults
     '(;; Use 8-bit encoding for readable files.
       ("text/x-patch" 8bit)
       ("text/.*" 8bit)
       ("message/rfc822" 8bit)
       ("application/emacs-lisp" 8bit)
       ("application/x-emacs-lisp" 8bit)
       ("application/x-patch" 8bit)
       (".*" base64))))

  (use-package mm-util
    :ensure nil
    :config
    (when (eq default-terminal-coding-system 'utf-8)
      (validate-setq mm-coding-system-priorities '(utf-8))))

  (use-package smime
    :ensure nil
    :config
    (let ((ca-directory "/etc/ssl/certs"))
      (when (file-exists-p ca-directory)
        (validate-setq smime-CA-directory ca-directory)))
    (let ((certificate-directory (path-join *user-home-directory* ".ssl")))
      (when (file-exists-p certificate-directory)
        (validate-setq
         ;; User certificate store.
         smime-certificate-directory certificate-directory))))

  (use-package mml-smime
    :ensure nil
    :config
    (validate-setq
     ;; Use EasyPG for signing and encryption of emails.
     mml-smime-use 'epg))

  (with-executable 'gpgsm
    (quelpa '(jl-encrypt
              :fetcher url
              :url "http://www.informationelle-selbstbestimmung-im-internet.de/emacs/jl-encrypt4.3/jl-encrypt.el"))
    (use-package jl-smime
      :requires jl-encrypt
      :quelpa (jl-smime
               :fetcher url
               :url "http://www.informationelle-selbstbestimmung-im-internet.de/emacs/jl-smime3.3/jl-smime.el")
      :init
      (eval-after-load 'gnus '(load "jl-smime"))))

  (use-package nnir
    :ensure nil
    :config
    (when (eq system-type 'darwin)
      (use-package nnir-spotlight
        :quelpa (nnir-spotlight
                 :fetcher github
                 :repo "renard/nnir-spotlight")
        :config
        ;; Enable Spotlight search on Darwin.
        (require 'nnir-spotlight)
        (validate-setq
         nnir-method-default-engines
         '((nnmaildir . spotlight)
           (nnml . spotlight)
           (nntp . gmane))
         nnir-spotlight-prefix message-directory)))

    (with-executable 'swish-e
      (validate-setq
       ;; Set search engine to Swish-E.
       nnir-search-engine 'swish-e
       nnir-swish-e-index-files (list
                                 (path-join *user-gnus-data-directory* "index.swish")))

      (defun user/swish-e-create/update-mail-index ()
        "Create or update mail index using Swish-E."
        (interactive)
        (eq
         (call-process-shell-command "swish-e" nil nil nil "-i" message-directory
                                     "-f" (first nnir-swish-e-index-files) "-e")
         0))))

  ;; Initialize Gnus modules.
  (require 'nnir))


(provide 'apps/gnus)
;;; gnus.el ends here
