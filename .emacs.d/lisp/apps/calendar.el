;;; calendar.el --- Emacs calendar and diary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-excorporate-cache-directory*
  (path-join *user-cache-directory* "excorporate")
  "Path to user's org data store.")

(defun user--calendar-load-hook ()
  "Calendar initialization hook."
  (with-feature 'appt
    ;; Enable appointment notifications.
    (appt-activate 1))

  (when (and (boundp 'excorporate-configuration) excorporate-configuration)
    ;; If excorporate has been configured make sure it's up and running.
    (excorporate)
    (excorporate-diary-enable)))


(defun user--exco-agenda-update-diary ()
  "Call excorporate to update the diary for today."
  (with-feature 'excorporate-diary
    (when exco--connections
      (exco-diary-diary-advice (calendar-current-date) (calendar-current-date) #'message "diary updated"))))


(defun user/swedish-easter (year)
  "Calculate the date for Easter in YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))


(defun user--swedish-holidays-config ()
  "Initialize swedish holidays."
  (validate-setq
   ;; General swedish holidays.
   holiday-general-holidays
   '((holiday-fixed 1 1 "New Year's Day")
     (holiday-fixed 5 1 "International Workers' Day")
     (holiday-fixed 12 31 "New Year's Eve"))
   ;; Christian holidays.
   holiday-christian-holidays
   '(;; Trettondedag jul
     (holiday-fixed 1 6 "Epiphany")
     (holiday-filter-visible-calendar
      (mapcar
       (lambda (dag)
         (list (calendar-gregorian-from-absolute
                (+ (user/swedish-easter displayed-year) (car dag)))
               (cadr dag)))
       '(;; Långfredagen
         (  -2 "Good Friday")
         ;; Påskafton
         (  -1 "Easter Eve")
         ;; Påskdagen
         (   0 "Easter Day")
         ;; Annandag påsk
         (  +1 "Easter Monday")
         ;; Kristi himmelfärdsdag
         ( +39 "Ascension Day")
         ;; Pingstafton
         ( +48 "Whitsun Eve")
         ;; Pingstdagen
         ( +49 "Whitsunday"))))
     ;; Alla helgons dag
     (holiday-filter-visible-calendar
      (list
       (list
        (calendar-gregorian-from-absolute
         (calendar-dayname-on-or-before
          6 (calendar-absolute-from-gregorian
             (list 11 6 displayed-year))))
        "All Saints' Day")))
     (holiday-fixed 12 24 "Christmas Eve")
     (holiday-fixed 12 25 "Christmas Day")
     (holiday-fixed 12 26 "Second Day of Christmas"))
   ;; Local holidays.
   holiday-local-holidays
   '((holiday-fixed 6 6 "National Day of Sweden")
     (let ((midsommar-d (calendar-dayname-on-or-before
                         6 (calendar-absolute-from-gregorian
                            (list 6 26 displayed-year)))))
       (holiday-filter-visible-calendar
        (list
         (list
          (calendar-gregorian-from-absolute (1- midsommar-d))
          "Midsummer Eve")
         (list
          (calendar-gregorian-from-absolute midsommar-d)
          "Midsummer Day")))))
   ;; Non-holiday special days.
   holiday-other-holidays
   '((holiday-fixed 2 14 "Valentine's Day")
     (holiday-fixed 4 1 "April Fools' Day")
     ;; Valborgsmässoafton
     (holiday-fixed 4 30 "Walpurgis Night")
     (holiday-float 5 0 -1 "Mother's Day")
     (holiday-filter-visible-calendar
      (mapcar
       (lambda (dag)
         (list (calendar-gregorian-from-absolute
                (+ (user/swedish-easter displayed-year) (car dag)))
               (cadr dag)))
       '(;; Annandag pingst
         ( +50 "Whitmonday"))))
     (holiday-float 11 0 2 "Father's Day")
     (holiday-fixed 12 10 "Nobel Day")
     (holiday-fixed 12 13 "Lucia")))

  (validate-setq
   ;; List of holidays to display in calendar.
   calendar-holidays
   (append holiday-general-holidays
           holiday-christian-holidays
           ;; Not holidays.
           ;;holiday-other-holidays
           ;;holiday-solar-holidays
           )))


(defun user--calendar-week-config ()
  "Initialize display of week numbers in calendar."
  (copy-face 'calendar-weekend-header 'calendar-iso-week-header-face)
  (copy-face 'calendar-weekend-header 'calendar-iso-week-face)

  (validate-setq
   ;; Week header.
   calendar-intermonth-header
   (propertize "Wk" 'font-lock-face 'calendar-iso-week-header-face)
   ;; Week display.
   calendar-intermonth-text
   '(propertize
     (format "%2d"
             (car
              (calendar-iso-from-absolute
               (calendar-absolute-from-gregorian (list month day year)))))
     'font-lock-face 'calendar-iso-week-face)))

(use-package calendar
  :commands calendar
  :defer
  :hook ((calendar-today-visible-hook . calendar-mark-today)
         (calendar-load-hook . user--calendar-load-hook))
  :config
  (let ((diary-data-store (path-join *user-org-data-directory* "diary.org")))
    (validate-setq
     ;; Date style should be ISO.
     calendar-date-style 'iso
     ;; Diary data store.
     diary-file diary-data-store
     ;; Always show entries for current date.
     calendar-view-diary-initially-flag t
     ;; Mark dates containing entries.
     calendar-mark-diary-entries-flag t
     ;; Mark holidays in calendar.
     calendar-mark-holidays-flag t
     ;; Week starts on Monday.
     calendar-week-start-day 1))

  (user--calendar-week-config)

  (use-package holidays
    :ensure calendar
    :config
    (user--swedish-holidays-config))

  (use-package diary-lib
    :ensure calendar
    :hook (diary-display-function . diary-fancy-display)
    :config
    (validate-setq
     ;; Display one week of entries from current date.
     diary-number-of-entries 7)

    (use-package appt
      :ensure calendar
      :config
      (validate-setq
       ;; Number of minutes to notify before an event starts.
       appt-message-warning-time 5
       appt-display-interval 5
       ;; Show time until next event in mode-line.
       appt-display-mode-line t
       ;; Appointment display type.
       appt-display-format 'window))))

(use-package calfw
  :defer
  :config
  (validate-setq
   ;; Use `fill-region' to wrap long lines.
   cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap
   ;; Use unicode to render calendar
   cfw:fchar-junction ?╋
   cfw:fchar-vertical-line ?┃
   cfw:fchar-horizontal-line ?━
   cfw:fchar-left-junction ?┣
   cfw:fchar-right-junction ?┫
   cfw:fchar-top-junction ?┯
   cfw:fchar-top-left-corner ?┏
   cfw:fchar-top-right-corner ?┓)

  (use-package calfw-org))

(use-package excorporate
  :ensure t
  :config
  (use-package excorporate-diary
    :ensure nil
    :after org
    :hook (org-agenda-cleanup-fancy-diary-hook
           . user--exco-agenda-update-diary)
    :init
    (make-directory *user-excorporate-cache-directory* t)
    :config
    (validate-setq
     ;; Set paths to excorporate data stores.
     excorporate-diary-today-file
     (path-join *user-excorporate-cache-directory* "diary-today")
     excorporate-diary-transient-file
     (path-join *user-excorporate-cache-directory* "diary-transient"))

    (excorporate-diary-enable)))


(provide 'apps/calendar)
;;; calendar.el ends here
