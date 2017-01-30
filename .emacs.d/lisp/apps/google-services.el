;;; google-services.el --- Support for Google services.
;;; Commentary:
;;; Code:

(defconst *user-google-services-data-directory*
  (path-join *user-data-directory* "google")
  "Path to user's Google services data store.")


(defun user--google-calendar-config ()
  "Initialize Google Calendar."
  (setq-default
   google-calendar/calendars-files
   (path-join *user-google-services-data-directory* "calendar.org")))


(defun user--google-services-config ()
  "Initialize Google services."
  (setq-default
   ;; Store OAuth2 token in data store.
   oauth2-token-file
   (path-join *user-google-services-data-directory* "oauth2.plstore"))

  (use-package google-contacts
    :defer t)
  (require-package '(:name google-calendar :after (user--google-calendar-config))))

(user--google-services-config)


(provide 'apps/google-services)
;;; google-services.el ends here
