;;; google-services.el --- Support for Google services.
;;; Commentary:
;;; Code:

(defconst *user-google-services-data-directory*
  (path-join *user-data-directory* "google")
  "Path to user's Google services data store.")


(defun user--google-calendar-config ()
  "Initialize Google Calendar."
  (after-load 'google-calendar
    (validate-setq
     google-calendar/calendars-files
     (path-join *user-google-services-data-directory* "calendar.org"))))


(use-package oauth2
  :defer t
  :config
  (validate-setq
   ;; Store OAuth2 token in data store.
   oauth2-token-file
   (path-join *user-google-services-data-directory* "oauth2.plstore")))

(use-package google-contacts
  :defer t)

(require-package
 '(:name google-calendar :after (user--google-calendar-config)))


(provide 'apps/google-services)
;;; google-services.el ends here
