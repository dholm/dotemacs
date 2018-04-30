 ;;; google-services.el --- Support for Google services. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-google-services-data-directory*
  (path-join *user-data-directory* "google")
  "Path to user's Google services data store.")


(use-package oauth2
  :defer
  :config
  (let ((plstore (path-join *user-google-services-data-directory*
                            "oauth2.plstore")))
    (when (file-exists-p plstore)
      (validate-setq
       ;; Store OAuth2 token in data store.
       oauth2-token-file plstore))))

(use-package google-contacts
  :defer)


(provide 'apps/google-services)
;;; google-services.el ends here
