;;; erc.el --- Internet relay chat client
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


(defun user--erc-mode-hook ()
  "Mode hook for erc.")


(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))


(defun user/erc-global-notify (&optional match-type nick message)
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

(defun user--erc-alert-config ()
  "Initialize alert for ERC."
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
  (alert-add-rule :mode 'erc-mode :style 'ignore :append t))


(defun user--erc-config ()
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
   erc-current-nick-highlight-type 'nick-or-keyword
   ;; Set the width to half the window width.
   erc-fill-column (/ (window-total-width (frame-root-window)) 2)
   ;; Modify the face of the header line when disconnected.
   erc-header-line-face-method 'erc-update-header-line-show-disconnected)

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

    (when (feature-p 'erc-colorize)
      ;; Highlight nicknames in chats.
      (add-to-list 'erc-modules 'colorize))

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
        (add-to-list 'erc-modules 'tex))

      (when (feature-p 'erc-image)
        ;; Render posted images inline in buffer.
        (add-to-list 'erc-modules 'image))))

  (when (eq default-terminal-coding-system 'utf-8)
    (setq-default
     ;; As long as the terminal handles it, force UTF-8.
     erc-server-coding-system '(utf-8 . utf-8)))

  ;; Ensure that the log directory exists.
  (make-directory erc-log-channels-directory t)
  (set-file-modes erc-log-channels-directory #o0700)

  (after-load 'alert
    (user--erc-alert-config))

  ;; Hooks.
  (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
  (add-hook 'erc-mode-hook 'user--erc-mode-hook)
  (when (feature-p 'bbdb2erc)
    (add-hook 'bbdb-notice-hook 'bbdb2erc-online-status))
  ;; Notification on important events.
  (add-hook 'erc-text-matched-hook 'user/erc-global-notify)
  (add-hook 'erc-insert-modify-hook 'user/erc-global-notify)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :irc 'erc))

(req-package erc
  :config (user--erc-config))
(req-package erc-colorize)
(req-package erc-track-score)
(req-package erc-image)
(req-package erc-view-log
  :loader :el-get)
(req-package erc-crypt)
(when (display-graphic-p)
  (req-package erc-tex))
(when (feature-p 'bbdb)
  (req-package bbdb2erc))
(with-executable 'bitlbee
  (after-load 'prodigy
    (prodigy-define-service
     :name "Bitlbee"
     :command "bitlbee"
     :args '("-n" "-F" "-v" "-d" "~/.bitlbee" "-c" "~/.bitlbee/bitlbee.conf")
     :cwd "~/.bitlbee")))


(provide 'apps/erc)
;;; erc.el ends here
