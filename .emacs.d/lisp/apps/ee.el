;;; ee.el --- Emacs information browser.
;;; Commentary:
;;; Code:

(defconst *user-ee-data-directory*
  (path-join *user-data-directory* "ee")
  "Path to user's ee data store.")


(defun user--ee-config ()
  "Initialize ee."
  (after-load 'ee
    (validate-setq
     ;; Database storage location.
     ee-data-directory *user-ee-data-directory*
     ee-view-data-directory *user-ee-data-directory*))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :information-db 'ee))

(require-package '(:name ee :after (user--ee-config)))


(provide 'apps/ee)
;;; ee.el ends here
