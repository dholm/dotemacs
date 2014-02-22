;;; deft.el --- sets up Deft
;;; Commentary:
;;; Code:

(defun user/deft-mode-hook ()
  "Deft mode hook."
  ;;; (Bindings) ;;;
  (user/bind-key-local :basic :open-file-context 'deft-find-file))


(defun user/deft-init ()
  "Initialize deft."
  (setq-default
   ;; The path to where notes will be stored.
   deft-directory (path-join *user-documents-directory* "Notes")
   ;; Set the default note format.
   deft-extension "org"
   deft-text-mode 'org-mode
   ;; Enforce good file naming.
   deft-use-filename-as-title t
   ;; Auto-save idle interval in seconds.
   deft-auto-save-interval 30.0)

  (add-hook 'deft-mode-hook 'user/deft-mode-hook)

  (after-load 'deft
    ;; Run deft's setup after it has been loaded.
    (deft-setup))

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :notes 'deft))

(require-package '(:name deft :after (user/deft-init)))


(provide 'apps/deft)
;;; deft.el ends here
