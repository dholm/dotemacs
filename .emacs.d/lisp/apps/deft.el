;;; deft.el --- sets up Deft
;;; Commentary:
;;; Code:

(defconst *user-notes-data-directory*
  (path-join *user-documents-directory* "Notes")
  "Path to user's notes data store.")

(defun user--deft-mode-hook ()
  "Deft mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :basic :open-file-context 'deft-find-file))


(defun user--deft-config ()
  "Initialize deft."
  ;; Ensure that notes store exists.
  (make-directory *user-notes-data-directory* t)

  (setq-default
   ;; The path to where notes will be stored.
   deft-directory *user-notes-data-directory*
   ;; Set the default note format.
   deft-extension "org"
   deft-text-mode 'org-mode
   ;; Enforce good file naming.
   deft-use-filename-as-title t
   ;; Auto-save idle interval in seconds.
   deft-auto-save-interval 30.0)

  (add-hook 'deft-mode-hook 'user--deft-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :notes 'deft))

(req-package deft
  :config (user--deft-config))


(provide 'apps/deft)
;;; deft.el ends here
