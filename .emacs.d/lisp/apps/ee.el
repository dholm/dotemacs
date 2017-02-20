;;; ee.el --- Emacs information browser.
;;; Commentary:
;;; Code:

(defconst *user-ee-data-directory*
  (path-join *user-data-directory* "ee")
  "Path to user's ee data store.")

(use-package ee
  :defer t
  :quelpa (ee
           :fetcher git
           :url "http://git.savannah.gnu.org/r/ee.git")
  :config
  (validate-setq
   ;; Database storage location.
   ee-data-directory *user-ee-data-directory*
   ee-view-data-directory *user-ee-data-directory*)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :information-db 'ee))


(provide 'apps/ee)
;;; ee.el ends here
