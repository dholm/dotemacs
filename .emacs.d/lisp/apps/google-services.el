;;; google-services.el --- Support for Google services.
;;; Commentary:
;;; Code:

(defconst *user-google-services-data-directory*
  (path-join *user-data-directory* "google")
  "Path to user's Google services data store.")


(use-package oauth2
  :defer t
  :config
  (validate-setq
   ;; Store OAuth2 token in data store.
   oauth2-token-file
   (path-join *user-google-services-data-directory* "oauth2.plstore")))

(use-package google-contacts
  :defer t)

(use-package google-calendar
  :requires google-contacts json
  :quelpa (google-calendar :fetcher github :repo "bateast/google-calendar")
  :config
  (validate-setq
   google-calendar/calendars-files
   (path-join *user-google-services-data-directory* "calendar.org")))


(provide 'apps/google-services)
;;; google-services.el ends here
