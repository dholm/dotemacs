;;; org.el --- Org mode support
;;; Commentary:
;;; Code:

(defconst *user-org-data-directory*
  (path-join *user-data-directory* "org")
  "Path to user's org data store.")


(defun user/org-mode-hook ()
  "Org mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :context-promote 'org-shiftup)
  (user/bind-key-local :code :context-demote 'org-shiftdown))


(defun user/org-agenda-init ()
  "Initialize org agenda."
  (let ((agenda-data-store (path-join *user-org-data-directory* "agendas")))
    (setq-default
     ;; Agenda data store.
     org-agenda-files `(,agenda-data-store)
     ;; Start on Monday.
     org-agenda-start-on-weekday t
     ;; Don't display scheduled todos.
     org-agenda-todo-ignore-scheduled 'future
     ;; Don't show nested todos.
     org-agenda-todo-list-sublevels nil)

    ;; Ensure that agenda data store exists.
    (make-directory agenda-data-store t))

  (when (not noninteractive)
    ;; When running in batch, don't try to setup windows.
    (setq-default
     ;; Show agenda in current window.
     org-agenda-window-setup 'current-window))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :agenda 'org-agenda))


(defun user/org-mode-init ()
  "Initialize Lua mode."
  (setq-default
   ;; Notes data store.
   org-default-notes-file (path-join *user-org-data-directory* "notes.org")
   ;; Pressing return on a link follows it.
   org-return-follows-link t
   ;; Log time for TODO state changes.
   org-log-done 'time
   ;; Log time when rescheduling an entry.
   org-log-reschedule 'time
   org-log-redeadline 'time
   ;; Log drawer state changes.
   org-log-into-drawer t
   ;; Allow single letter commands at beginning of headlines.
   org-use-speed-commands t
   ;; Fontify code blocks by default.
   org-src-fontify-natively t
   ;; Tab should operate according to local mode.
   org-src-tab-acts-natively t)

  (when (not noninteractive)
    (setq-default
     ;; Remove clock line if time is zero.
     org-clock-out-remove-zero-time-clocks t
     ;; Stop clock when entry is marked as DONE.
     org-clock-out-when-done t
     ;; Show the amount of time spent on the current task today.
     org-clock-modeline-total 'today
     ;; Resume clock when reopening Emacs.
     org-clock-persist t)

    (after-load 'org-mode
      ;; Initialize clock persistence.
      (org-clock-persistence-insinuate)))

  (when (display-graphic-p)
    (setq-default
     ;; Display inline images when starting up.
     org-startup-with-inline-images t))

  (user/org-agenda-init)

  (add-hook 'org-mode-hook 'user/org-mode-hook))


(defun user/org-init ()
  "Initialize org mode."
  ;; Fix for EIN if org hasn't been setup yet.
  (autoload 'org-add-link-type "org" "" t)

  ;;; (Packages) ;;;
  (require-package '(:name org-mode :after (user/org-mode-init)))
  (require-package '(:name org-caldav)))

(user/org-init)


(provide 'modes/org)
;;; org.el ends here
