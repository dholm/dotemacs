;;; nsm.el --- Configure network security manager
;;; Commentary:
;;; Code:

(defconst *user-nsm-data-directory*
  (path-join *user-data-directory* "nsm")
  "Path to user's Wanderlust data store.")


(use-package nsm
  :defer t
  :init
  (make-directory *user-nsm-data-directory* t)
  :config
  (setq-default
   ;; Location of security manager settings.
   nsm-settings-file
   (path-join *user-nsm-data-directory* "network-security.data")))


(provide 'utilities/nsm)
;;; nsm.el ends here
