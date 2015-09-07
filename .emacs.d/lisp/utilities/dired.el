;;; dired.el --- Configuration for dired
;;; Commentary:
;;; Code:

(defun user/dired-mode-hook ()
  "Mode hook for dired."
  (with-feature 'emacs-async
    ;; Asynchronous operations in dired.
    (dired-async-mode t))

  (with-feature 'guide-key
    ;; dired specific key guides.
    (guide-key/add-local-guide-key-sequence "%")))


(defun user/dired-efap-init ()
  "Initialize dired-efap."
  (after-load 'dired
    ;; Load dired-efap when dired is loaded.
    (require 'dired-efap))

  ;;; (Bindings) ;;;
  (define-key dired-mode-map [R] 'dired-efap)
  (when (display-graphic-p)
    (define-key dired-mode-map [down-mouse-1] 'dired-efap-click)))


(defun user/dired-init ()
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
  (require-package '(:name dired-k))

  ;;; (Bindings) ;;;
  ;; Do not open new buffers when going down or up a directory.
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda ()
                                         (interactive)
                                         (find-alternate-file "..")))
  (when (display-graphic-p)
    (define-key dired-mode-map [double-mouse-1] 'dired-find-file))

  ;;; (Packages) ;;;
  (require-package '(:name emacs-async))
  (require-package '(:name dired-efap :after (user/dired-efap-init))))

(user/dired-init)


(provide 'utilities/dired)
;;; dired.el ends here
