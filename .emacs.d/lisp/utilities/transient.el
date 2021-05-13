;;; transient.el --- Configuration for transient. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package transient
  :ensure nil
  :config
  (validate-setq
   ;; Location of transient files.
   transient-history-file (path-join *user-cache-directory* "transient" "history.el")
   transient-values-file (path-join *user-cache-directory* "transient" "values.el")
   transient-levels-file (path-join *user-cache-directory* "transient" "levels.el"))

  ;; Create the transient cache folder.
  (make-directory (path-join *user-cache-directory* "transient") t))


(provide 'utilities/transient)
;;; transient.el ends here
