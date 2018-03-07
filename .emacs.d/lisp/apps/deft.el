;;; deft.el --- sets up Deft -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-notes-data-directory*
  (path-join *user-documents-directory* "Notes")
  "Path to user's notes data store.")

(use-package deft
  :commands deft
  :bind-wrap
  (((:key :apps :notes) . deft)
   :map deft-mode-map
   ((:key :basic :open-file-context) . deft-find-file))
  :init
  (make-directory *user-notes-data-directory* t)
  :config
  (validate-setq
   ;; The path to where notes will be stored.
   deft-directory *user-notes-data-directory*
   ;; Enforce good file naming.
   deft-use-filename-as-title t
   ;; Auto-save idle interval in seconds.
   deft-auto-save-interval 30.0)

  (use-package helm-deft
    :quelpa (helm-deft :fetcher github :repo "dfeich/helm-deft")
    :bind-wrap
    ((:key :apps :notes) . helm-deft)
    :config
    (validate-setq
     ;; Directories to use.
     helm-deft-dir-list `(,*user-notes-data-directory*))))

(provide 'apps/deft)
;;; deft.el ends here
