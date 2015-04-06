;;; calendar.el --- Emacs calendar and diary
;;; Commentary:
;;; Code:

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


(defun user/swedish-holidays-init ()
  "Initialize swedish holidays."
  (setq-default
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

  (setq-default
   ;; List of holidays to display in calendar.
   calendar-holidays
   (append holiday-general-holidays
           holiday-christian-holidays
           ;; Not holidays.
           ;;holiday-other-holidays
           ;;holiday-solar-holidays
           )))


(defun user/calfw-init ()
  "Initialize calfw."
  (setq-default
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
   cfw:fchar-top-right-corner ?┓))


(defun user/calendar-init ()
  "Initialize calendar."
  (let ((diary-data-store (path-join *user-org-data-directory* "diary.org")))
    (setq-default
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
     ;; Display one week of entries from current date.
     diary-number-of-entries 7
     ;; Week starts on Monday.
     calendar-week-start-day 1))

  (user/swedish-holidays-init)

  ;;; (Hooks) ;;;
  (add-hook 'diary-display-function 'diary-fancy-display)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  ;;; (Packages) ;;;
  (require-package '(:name calfw :after (user/calfw-init)))
  (require-package '(:name excorporate)))

(user/calendar-init)


(provide 'apps/calendar)
;;; calendar.el ends here
