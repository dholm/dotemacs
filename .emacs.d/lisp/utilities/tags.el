;;; tags.el --- Helpers for working with tags
;;; Commentary:
;;; Code:

(defconst *user-tags-cache-directory*
  (path-join *user-cache-directory* "tags")
  "Base path to user's tag cache store.")


(defun user/follow-tag ()
  "Follow tag at point using best available method."
  (interactive)
  (let ((start-point (point)))
    (when (and (user/gnu-global-tags-p) (boundp 'helm-gtags-mode) helm-gtags-mode)
      (call-interactively 'helm-gtags-find-tag))
    (when (and (eq start-point (point)) (boundp 'semantic-mode) semantic-mode)
      (call-interactively 'semantic-ia-fast-jump))))


(defun user/tags-location (&optional path)
  "Get the path to the tag store for PATH, or the current buffer if nil."
  (with-project-root project-root path
    (path-join *user-tags-cache-directory* (substring project-root 1))))


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
