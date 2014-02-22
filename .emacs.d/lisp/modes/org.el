;;; org.el --- Org mode support
;;; Commentary:
;;; Code:

(defun user/org-mode-hook ()
  "Org mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :code :context-promote 'org-shiftup)
  (user/bind-key-local :code :context-demote 'org-shiftdown))


(defun user/org-mode-init ()
  "Initialize Lua mode."
  (setq-default
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
      (org-clock-persistence-insinuate))))

  (when (display-graphic-p)
    (setq-default
     ;; Display inline images when starting up.
     org-startup-with-inline-images t)

  (add-hook 'org-mode-hook 'user/org-mode-hook))

(require-package '(:name org-mode :after (user/org-mode-init)))

;; Fix for EIN if org hasn't been setup yet.
(autoload 'org-add-link-type "org" "" t)


(provide 'modes/org)
;;; org.el ends here
