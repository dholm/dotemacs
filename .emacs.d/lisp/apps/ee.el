;;; ee.el --- Emacs information browser. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-ee-data-directory*
  (path-join *user-data-directory* "ee")
  "Path to user's ee data store.")

(use-package ee
  :quelpa (ee
           :fetcher git
           :url "http://git.savannah.gnu.org/r/ee.git")
  :commands ee
  :init
  (user/bind-key-global :apps :information-db 'ee)
  :config
  (validate-setq
   ;; Database storage location.
   ee-data-directory *user-ee-data-directory*
   ee-view-data-directory *user-ee-data-directory*))


(provide 'apps/ee)
;;; ee.el ends here
