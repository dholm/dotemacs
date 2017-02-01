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
  "Configure deft."
  ;; Ensure that notes store exists.
  (make-directory *user-notes-data-directory* t)

  (validate-setq
   ;; The path to where notes will be stored.
   deft-directory *user-notes-data-directory*
   ;; Enforce good file naming.
   deft-use-filename-as-title t
   ;; Auto-save idle interval in seconds.
   deft-auto-save-interval 30.0)

  ;;; (Hook) ;;;
  (add-hook 'deft-mode-hook 'user--deft-mode-hook))


(defun user--deft-init ()
  "Initialize deft."
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :notes 'deft))


(use-package deft
  :commands deft
  :init (user--deft-init)
  :config (user--deft-config))


(provide 'apps/deft)
;;; deft.el ends here
