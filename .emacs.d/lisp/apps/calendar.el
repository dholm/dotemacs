;;; calendar.el --- Emacs calendar and diary
;;; Commentary:
;;; Code:

(defun user/calendar-init ()
  "Initialize calendar."
  (let ((diary-data-store (path-join *user-org-data-directory* "diary.org")))
    (setq-default
     ;; Diary data store.
     diary-file diary-data-store
     ;; Always show entries for current date.
     calendar-view-diary-initially-flag t
     ;; Mark dates containing entries.
     calendar-mark-diary-entries-flag t
     ;; Display one week of entries from current date.
     diary-number-of-entries 7
     ;; Week starts on Monday.
     calendar-week-start-day 1))

  ;;; (Hooks) ;;;
  (add-hook 'diary-display-function 'diary-fancy-display)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(user/calc-init)


(provide 'apps/calendar)
;;; calendar.el ends here
