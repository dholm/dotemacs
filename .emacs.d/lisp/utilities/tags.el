;;; tags.el --- Helpers for working with tags
;;; Commentary:
;;; Code:

(defun user/follow-tag ()
  "Follow tag at point using best available method."
  (interactive)
  (let ((start-point (point)))
    (when (and (boundp 'helm-gtags-mode) helm-gtags-mode)
      (call-interactively 'helm-gtags-find-tag))
    (when (and (eq start-point (point)) (boundp 'semantic-mode) semantic-mode)
      (call-interactively 'semantic-ia-fast-jump))))


(defun user/tags-init ()
  "Initialize tag support."
  ;;; (Bindings) ;;;
  (user/bind-key-global :nav :follow-symbol 'user/follow-tag)

  ;;; (Packages) ;;;
  (when (feature-p 'helm)
    (require-package '(:name helm-etags-plus))))

(user/tags-init)


(provide 'utilities/tags)
;;; tags.el ends here
