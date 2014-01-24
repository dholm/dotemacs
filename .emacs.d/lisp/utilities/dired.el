;;; dired.el --- Configuration for dired
;;; Commentary:
;;; Code:

(defun user/dired-init ()
  "Initialize dired."
  (setq-default
   ;; Always copy recursively without asking.
   dired-recursive-copies 'always
   ;; Ask once when recursively deleting a directory.
   dired-recursive-deletes 'top
   ;; Allow dired to be smart about operations.
   dired-dwim-target t)

  ;;; (Bindings) ;;;
  (after-load 'dired
    ;; Do not open new buffers when going down or up a directory.
    (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "^") (lambda ()
                                           (interactive)
                                           (find-alternate-file "..")))))

(user/dired-init)


(provide 'utilities/dired)
;;; dired.el ends here
