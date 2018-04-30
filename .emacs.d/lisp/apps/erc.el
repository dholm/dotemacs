;;; erc.el --- Internet relay chat client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-erc-cache-directory*
  (path-join *user-cache-directory* "erc")
  "Path to user's erc cache store.")

(defvar *user-erc-noise-regexp*
  "\\(Logging in:\\|Signing off\\|You're now away\\|Welcome back\\|Setting automatically away\\)"
  "This regexp matches unwanted noise.")


(defface erc-header-line-disconnected
  '((t (:inherit 'flymake-errline)))
  "Face to use when ERC has been disconnected."
  :group 'erc-faces)


(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))


(defun user--erc-global-notify (&optional match-type nick message)
  "Global notification on MATCH-TYPE when NICK sent a MESSAGE to user."
  (when (or (null match-type) (not (eq match-type 'fool)))
    (if (feature-p 'alert)
        (let (alert-log-messages)
          (alert (or message (buffer-string))
                 :severity 'high
                 :title (concat "ERC: " (or nick (buffer-name)))
                 :data message))
      (notifications-notify
       :title nick
       :body message
       :urgency 'low))))

(defun user/erc-prompt ()
  "ERC prompt function."
  (if (and (boundp 'erc-default-recipients)
           (erc-default-target))
      (erc-propertize (concat "[" (erc-default-target) "]")
                      'read-only t 'rear-nonsticky t
                      'front-nonsticky t)
    (erc-propertize (concat "[" (buffer-name) "]") 'read-only t
                    'rear-nonsticky t 'front-nonsticky t)))


(defun user/erc-alert-important-p (info)
  "Check if ERC alert is important based on INFO."
  (let ((msg (plist-get info :message))
        (erc-message (plist-get info :data)))
    (and erc-message
         (not (or (string-match "^\\** *Users on #" msg)
                  (string-match *user-erc-noise-regexp* msg))))))

(use-package erc
  :commands erc
  :hook ((erc-connect-pre-hook . (lambda (x) (erc-update-modules)))
         (erc-insert-modify-hook . user--erc-global-notify))
  :init
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :irc 'erc)
  :config
  (validate-setq
   ;; Close ERC buffers on quit.
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t
   ;; Open queries in the current window.
   erc-query-display 'buffer

   ;; Nicer ERC prompt.
   erc-prompt 'user/erc-prompt
   ;; ERC mode and header line.
   erc-mode-line-format
   (concat
    "[" (with-face "%n" 'erc-my-nick-face) "] "
    "[" (with-face "%S" 'erc-keyword-face) "(%m)] "
    (with-face "%a" 'erc-bold-face))
   erc-header-line-format "%o"
   ;; Modify the face of the header line when disconnected.
   erc-header-line-face-method 'erc-update-header-line-show-disconnected)

  (use-package erc-backend
    :ensure nil
    :config
    ;; validate-setq fails for erc-backend for some reason.
    (setq
     ;; Attempt to reconnect forever.
     erc-server-reconnect-attempts t)

    (when (eq default-terminal-coding-system 'utf-8)
      (setq
       ;; As long as the terminal handles it, force UTF-8.
       erc-server-coding-system '(utf-8 . utf-8))))

  (use-package erc-goodies
    :ensure nil
    :config
    (validate-setq
     ;; Interpret mIRC color codes.
     erc-interpret-mirc-color t
     ;; Ensure prompt is at the bottom of the window.
     erc-scrolltobottom-mode t)

    (add-to-list
     'erc-modules
     ;; Leave point above un-viewed text in channels.
     'keep-place))

  (use-package erc-log
    :ensure nil
    :config
    (validate-setq
     ;; Insert log file contents into opened buffers.
     erc-log-insert-log-on-open t
     ;; Save buffers to log on activity.
     erc-log-write-after-send t
     erc-log-write-after-insert t
     ;; Path to log store.
     erc-log-channels-directory (path-join *user-erc-cache-directory* "logs"))

    (make-directory erc-log-channels-directory t)
    (set-file-modes erc-log-channels-directory #o0700)

    (use-package erc-view-log
      :quelpa (erc-view-log
               :fetcher github
               :repo "Niluge-KiWi/erc-view-log")))

  (use-package erc-stamp
    :ensure nil
    :config
    (validate-setq
     ;; Always add a timestamp.
     erc-timestamp-only-if-changed-flag nil))

  (use-package erc-truncate
    :ensure nil
    :config
    (validate-setq
     ;; Maximum buffer size.
     erc-max-buffer-size 100000))

  (use-package erc-button
    :ensure nil
    :config
    (validate-setq
     ;; Even buttonize links that are not proper URLs.
     erc-button-url-regexp
     "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"))

  (use-package erc-match
    :ensure nil
    :hook (erc-text-matched-hook . user--erc-global-notify)
    :config
    (validate-setq
     ;; Highlight user mentioning us or a keyword.
     erc-current-nick-highlight-type 'nick-or-keyword))

  (use-package erc-fill
    :ensure nil
    :config
    (validate-setq
     ;; Set the width to half the window width.
     erc-fill-column (/ (window-total-width (frame-root-window)) 2)))

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

  (when (display-graphic-p)
    ;; Replace smileys with icons.
    (add-to-list 'erc-modules 'smiley))

  (use-package erc-colorize
    :init
    ;; Highlight nicknames in chats.
    (add-to-list 'erc-modules 'colorize))

  (use-package erc-track
    :ensure nil
    :config
    (use-package erc-track-score
      :config
      (validate-setq
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
       ;; Show channel score.
       erc-track-showcount t)

      (erc-track-score-mode t)))

  (use-package erc-services
    :ensure nil
    :config
    (add-to-list
     'erc-nickserv-alist
     ;; Don't talk to nickserv if server is unknown.
     `(Unknown nil nil ,(getenv "USER") "IDENTIFY" nil nil nil)))

  (use-package erc-image
    :if window-system
    :config
    ;; Render posted images inline in buffer.
    (add-to-list 'erc-modules 'image))

  (use-package erc-crypt)

  (use-package erc-tex
    :if window-system
    :quelpa (erc-tex
             :fetcher github
             :repo "davazp/erc-tex")
    :init
    ;; Render (La)TeX mathematical expressions.
    (add-to-list 'erc-modules 'tex))

  (use-package erc-scrolltoplace
    :config
    (add-to-list
     'erc-modules
     ;; Improved keep-place by fitting more unread text.
     'scrolltoplace))

  (use-package erc-status-sidebar)

  (when (feature-p 'bbdb)
    (use-package bbdb2erc
      :hook (bbdb-notice-hook . bbdb2erc-online-status)))

  (with-executable 'bitlbee
    (with-eval-after-load 'prodigy
      (prodigy-define-service
       :name "Bitlbee"
       :command "bitlbee"
       :args '("-n" "-F" "-v" "-d" "~/.bitlbee" "-c" "~/.bitlbee/bitlbee.conf")
       :cwd "~/.bitlbee")))

  (with-feature 'alert
    ;; Notify user on important events if ERC buffer isn't visible.
    (alert-add-rule :status 'buried
                    :mode   'erc-mode
                    :predicate #'user/erc-alert-important-p
                    :style (user/alert-style)
                    :append t)
    ;; Log all important ERC events to the alert log.
    (alert-add-rule :mode 'erc-mode
                    :predicate #'user/erc-alert-important-p
                    :style 'log
                    :append t)
    ;; Ignore remaining events.
    (alert-add-rule :mode 'erc-mode :style 'ignore :append t)))


(provide 'apps/erc)
;;; erc.el ends here
