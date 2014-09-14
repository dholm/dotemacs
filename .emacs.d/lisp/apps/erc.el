;;; erc.el --- Internet relay chat client
;;; Commentary:
;;; Code:

(defconst *user-erc-cache-directory*
  (path-join *user-cache-directory* "erc")
  "Path to user's erc cache store.")


(defun user/erc-mode-hook ()
  "Mode hook for erc.")


(defun user/erc-global-notify (match-type nick message)
  "Global notification on MATCH-TYPE when NICK sent a MESSAGE to user."
  (notifications-notify
   :title nick
   :body message
   :urgency 'low))


(defun user/bitlbee-init ()
  "Initialize bitlbee."
  (setq-default
   ;; Run in ForkDaemon mode so that purple plugins work.
   bitlbee-options "-n -F -v "))


(defun user/erc-prompt ()
  "ERC prompt function."
  (if (and (boundp 'erc-default-recipients)
           (erc-default-target))
      (erc-propertize (concat "[" (erc-default-target) "]")
                      'read-only t 'rear-nonsticky t
                      'front-nonsticky t)
    (erc-propertize (concat "[" (buffer-name) "]") 'read-only t
                    'rear-nonsticky t 'front-nonsticky t)))


(defun user/erc-init ()
  "Initialize erc."
  (setq-default
   ;; Close ERC buffers on quit.
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t
   ;; Interpret mIRC color codes.
   erc-interpret-mirc-color t
   ;; Open queries in the current window.
   erc-query-display 'buffer
   ;; Exclude the server buffer when tracking.
   erc-track-exclude-server-buffer t
   ;; Non-interesting events.
   erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                             "324" "329" "332" "333" "353" "477")
   ;; Track all priority faces.
   erc-track-priority-faces-only 'all
   ;; List of priority faces to track.
   erc-track-faces-priority-list
   '(erc-error-face
     erc-current-nick-face
     erc-keyword-face
     erc-nick-msg-face
     erc-direct-msg-face
     erc-dangerous-host-face
     erc-notice-face
     erc-prompt-face)
   ;; Path to log store.
   erc-log-channels-directory (path-join *user-erc-cache-directory* "logs")
   ;; Log channels.
   erc-log-channels t
   ;; Insert log file contents into opened buffers.
   erc-log-insert-log-on-open t
   ;; Save buffers to log on activity.
   erc-log-write-after-send t
   erc-log-write-after-insert t
   ;; Always add a timestamp.
   erc-timestamp-only-if-changed-flag nil
   ;; Ensure prompt is at the bottom of the window.
   erc-scrolltobottom-mode t
   ;; Maximum buffer size.
   erc-max-buffer-size 100000
   ;; Nicer ERC prompt.
   erc-prompt 'user/erc-prompt
   ;; Even buttonize links that are not proper URLs.
   erc-button-url-regexp
   "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"
   ;; ERC mode and header line.
   erc-mode-line-format
   (concat
    "[" (with-face "%n" 'erc-my-nick-face) "] "
    "[" (with-face "%S" 'erc-keyword-face) "(%m)] "
    (with-face "%a" 'erc-bold-face))
   erc-header-line-format "%o"
   ;; Highlight user mentioning us or a keyword.
   erc-current-nick-highlight-type 'nick-or-keyword)

  (after-load 'erc
    (add-many-to-list
     'erc-modules
     ;; Automatically join channels on connect.
     'autojoin
     ;; Automatically go away when inactive.
     'autoaway
     ;; Handle IRC control characters.
     'irccontrols
     ;; Channel listings.
     'list
     ;; Automatically log buffers.
     'log
     ;; Highlight certain keywords.
     'match
     ;; Move to the prompt when typing.
     'move-to-prompt
     ;; Automatically detect netsplits.
     'netsplit
     ;; IRC networks data.
     'networks
     ;; Hide non-irc commands after evaluation.
     'noncommands
     ;; Generate notifications for select users.
     'notify
     ;; Make displayed lines read-only.
     'readonly
     ;; Input history ring.
     'ring
     ;; Notification on certain events.
     'notifications
     ;; Support for various services (i.e. NickServ).
     'services
     ;; Enable spell checking.
     'spelling
     ;; Timestamp messages.
     'stamp
     ;; Track interesting events.
     'track
     ;; Truncate really long irc buffers.
     'truncate)

    (when (feature-p 'erc-highlight-nicknames)
      ;; Highlight nicknames in chats.
      (add-to-list 'erc-modules 'highlight-nicknames))

    (when (feature-p 'erc-youtube)
      ;; Display information about YouTube links.
      (add-to-list 'erc-modules 'youtube))

    (when (feature-p 'erc-track-score)
      (setq-default
       ;; Show channel score.
       erc-track-showcount t)

      (after-load 'erc-track
        (erc-track-score-mode t)))

    (when (display-graphic-p)
      ;; Replace smileys with icons.
      (add-to-list 'erc-modules 'smiley)
      (when (feature-p 'erc-tex)
        ;; Render (La)TeX mathematical expressions.
        (add-to-list 'erc-modules 'tex))))

  (when (eq default-terminal-coding-system 'utf-8)
    (setq-default
     ;; As long as the terminal handles it, force UTF-8.
     erc-server-coding-system '(utf-8 . utf-8)))

  ;; Notification on personal messages.
  (add-hook 'erc-text-matched-hook 'user/erc-global-notify)

  ;; Ensure that the log directory exists.
  (make-directory erc-log-channels-directory t)
  (set-file-modes erc-log-channels-directory #o0700)

  ;; Hooks.
  (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
  (add-hook 'erc-mode-hook 'user/erc-mode-hook)
  (when (feature-p 'bbdb2erc)
    (add-hook 'bbdb-notice-hook 'bbdb2erc-online-status))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :irc 'erc))

(require-package '(:name erc :after (user/erc-init)))
(require-package '(:name erc-youtube))
(require-package '(:name erc-highlight-nicknames))
(require-package '(:name erc-track-score))
(require-package '(:name erc-view-log))
(require-package '(:name erc-crypt))
(when (display-graphic-p)
  (require-package '(:name erc-tex)))
(when (feature-p 'bbdb)
  (require-package '(:name bbdb2erc)))
(with-executable 'bitlbee
  (require-package '(:name bitlbee :after (user/bitlbee-init))))


(provide 'apps/erc)
;;; erc.el ends here
