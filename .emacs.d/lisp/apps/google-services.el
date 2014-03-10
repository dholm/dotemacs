;;; google-services.el --- Support for Google services.
;;; Commentary:
;;; Code:

(defconst *user-google-services-data-directory*
  (path-join *user-data-directory* "google")
  "Path to user's Google services data store.")


(defun user/google-contacts-message-mode-hook ()
  "Message mode hook for Google Contacts."
  ;; Support for address completion in message mode using Google Contacts.
  (require 'google-contacts-message))


(defun user/google-contacts-init ()
  "Initialize Google Contacts."
  (add-hook 'message-mode-hook 'user/google-contacts-message-mode-hook))


(defun user/google-calendar-init ()
  "Initialize Google Calendar."
  (setq-default
   google-calendar/calendars-files (path-join *user-google-services-data-directory* "calendar.org")))


(defun user/google-services-init ()
  "Initialize Google services."
  (setq-default
   ;; Store OAuth2 token in data store.
   oauth2-token-file (path-join *user-google-services-data-directory* "oauth2.plstore"))

  (require-package '(:name google-contacts :after (user/google-contacts-init)))
  (require-package '(:name google-calendar :after (user/google-calendar-init))))

(user/google-services-init)


(provide 'apps/google-services)
;;; google-services.el ends here
