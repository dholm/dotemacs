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


(defun user/erc-init ()
  "Initialize erc."
  (setq-default
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
   ;; Save buffer when parting channel.
   erc-save-buffer-on-part t
   ;; Always add a timestamp.
   erc-timestamp-only-if-changed-flag nil
   ;; Ensure prompt is at the bottom of the window.
   erc-scrolltobottom-mode t)

  (after-load 'erc
    (add-many-to-list
     'erc-modules
     ;; Automatically join channels on connect.
     'autojoin
     ;; Automatically go away when inactive.
     'autoaway
     ;; Automatic completion.
     'completion
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

    (when (display-graphic-p)
      ;; Replace smileys with icons.
      (add-to-list 'erc-modules 'smiley)))

  (when (eq default-terminal-coding-system 'utf-8)
    (setq-default
     ;; As long as the terminal handles it, force UTF-8.
     erc-server-coding-system '(utf-8 . utf-8)))

  ;; Notification on personal messages.
  (add-hook 'erc-text-matched-hook 'user/erc-global-notify)

  ;; Ensure that the log directory exists.
  (make-directory erc-log-channels-directory t)

  ;; Hooks.
  (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
  (add-hook 'erc-mode-hook 'user/erc-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :irc 'erc))

(require-package '(:name erc :after (user/erc-init)))
(require-package '(:name erc-highlight-nicknames))
(with-executable 'bitlbee
  (require-package '(:name bitlbee)))


(provide 'apps/erc)
;;; erc.el ends here
