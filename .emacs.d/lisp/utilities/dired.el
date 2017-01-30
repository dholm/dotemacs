;;; dired.el --- Configuration for dired
;;; Commentary:
;;; Code:

(defun user--dired-mode-hook ()
  "Mode hook for dired."
  (with-feature 'async
    ;; Asynchronous operations in dired.
    (dired-async-mode t))

  (with-feature 'guide-key
    ;; dired specific key guides.
    (guide-key/add-local-guide-key-sequence "%")))


(defun user--dired-efap-config ()
  "Initialize dired-efap."
  (after-load 'dired
    ;; Load dired-efap when dired is loaded.
    (require 'dired-efap))

  ;;; (Bindings) ;;;
  (define-key dired-mode-map [R] 'dired-efap)
  (when (display-graphic-p)
    (define-key dired-mode-map [down-mouse-1] 'dired-efap-click)))


(defun user--dired-config ()
  "Initialize dired."
  (setq-default
   ;; Always copy recursively without asking.
   dired-recursive-copies 'always
   ;; Ask once when recursively deleting a directory.
   dired-recursive-deletes 'top
   ;; Allow dired to be smart about operations.
   dired-dwim-target t
   ;; Default flags for ls.
   dired-listing-switches "-alh")

  ;;; (Packages) ;;;
  (use-package dired-k
    :defer t)

  ;;; (Bindings) ;;;
  ;; Do not open new buffers when going down or up a directory.
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))
  (when (display-graphic-p)
    (define-key dired-mode-map [double-mouse-1] 'dired-find-file))

  ;;; (Packages) ;;;
  (use-package async
    :defer t)
  (use-package dired-efap
    :defer t
    :config (user--dired-efap-config)))

(user--dired-config)


(provide 'utilities/dired)
;;; dired.el ends here
