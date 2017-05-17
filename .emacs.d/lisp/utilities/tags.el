;;; tags.el --- Helpers for working with tags -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user/use-rtags (&optional filemanager)
  "Check if rtags can be used, optionally with its FILEMANAGER."
  (and
   (fboundp 'rtags-executable-find)
   (rtags-executable-find "rc")
   (cond ((not (ggtags-current-project-root)) t)
         ((and (not (eq major-mode 'c++-mode))
               (not (eq major-mode 'c-mode)))
          (rtags-has-filemanager))
         (filemanager (rtags-has-filemanager))
         (t (rtags-is-indexed)))))


(defun user/use-helm-gtags ()
  "Check if helm-gtags can be used."
  (and (boundp 'helm-gtags-mode) helm-gtags-mode
       (boundp 'helm-gtags-tag-location) helm-gtags-tag-location))


(defun user/use-semantic ()
  "Check if semantic can be used."
  (and (boundp 'semantic-mode)
       semantic-mode))


(defun user/eval-until-move (cond-list)
  "Evaluate expressions from COND-LIST until point is changed."
  (let ((start-buffer (current-buffer))
        (start-point (point)))
    (dolist (cond-expr cond-list)
      (when (and (eq start-buffer (current-buffer))
                 (eq start-point (point)))
        (let ((cnd (first cond-expr))
              (expr (second cond-expr)))
          (when (eval cnd)
            (eval expr)))))))


(defun user/tag-update-index ()
  "Update tag index backend(s)."
  (interactive)
  (user/gnu-global-create/update))


(defun user/tag-toc ()
  "Show table of contents for current context."
  (interactive)
  (unless (and (user/use-rtags t)
               (rtags-imenu))
    (user/eval-until-move
     '(((or (user/use-helm-gtags) (user/use-semantic))
        (call-interactively 'helm-semantic-or-imenu))
       (t (call-interactively 'helm-imenu))))))


(defun user/tag-follow ()
  "Follow tag at point using best available method."
  (interactive)
  (unless (and (user/use-rtags)
               (boundp 'rtags-last-request-not-indexed)
               (not rtags-last-request-not-indexed)
               (rtags-find-symbol-at-point))
    (user/eval-until-move
     '(((eq major-mode 'emacs-lisp-mode)
        (call-interactively 'elisp-slime-nav-find-elisp-thing-at-point))
       ((user/use-helm-gtags)
        (call-interactively 'helm-gtags-dwim))
       ((user/use-semantic)
        (call-interactively 'semantic-ia-fast-jump))))))


(defun user/tag-references-at-point ()
  "Find references at current point."
  (interactive)
  (unless (and (user/use-rtags (and (boundp 'rtags-last-request-not-indexed)
                                    (not rtags-last-request-not-indexed)))
               (rtags-find-references-at-point))
    (user/eval-until-move
     '(((user/use-helm-gtags)
        (call-interactively 'helm-gtags-find-rtag))
       ((user/use-semantic)
        (call-interactively 'semantic-symref))))))


(defun user/tag-find ()
  "Find tag by name."
  (interactive)
  (unless (and (user/use-rtags)
               (rtags-find-symbol))
    (user/eval-until-move
     '(((user/use-helm-gtags)
        (call-interactively 'helm-gtags-select))
       ((user/use-semantic)
        (call-interactively 'semantic-symref-find-tags-by-regexp))))))


(defun user/tag-find-references ()
  "Find references to tag."
  (interactive)
  (unless (and (user/use-rtags)
               (rtags-find-references))
    (user/eval-until-move
     '(((user/use-helm-gtags)
        (call-interactively 'helm-gtags-find-rtag))
       ((user/use-semantic)
        (call-interactively 'semantic-symref-regexp))))))


(defun user/tag-find-virtuals ()
  "Find implementers of virtual function at point."
  (interactive)
  (unless (and (user/use-rtags)
               (boundp 'rtags-last-request-not-indexed)
               (not rtags-last-request-not-indexed)
               (rtags-find-virtuals-at-point))))


(defun user/tag-insert-dependency-from-point ()
  "Deduce and insert dependency for symbol at point."
  (interactive)
  (unless (and (user/use-rtags)
               (boundp 'rtags-last-request-not-indexed)
               (not rtags-last-request-not-indexed)
               (rtags-get-include-file-for-symbol))))


(defun user/tag-file-dependencies ()
  "List dependencies of current file."
  (interactive)
  (unless (and (user/use-rtags)
               (boundp 'rtags-last-request-not-indexed)
               (not rtags-last-request-not-indexed)
               (rtags-print-dependencies))))


(defun user/tag-find-file ()
  "Find file file using tags."
  (interactive)
  (unless (and (user/use-rtags t)
               (call-interactively 'rtags-find-file))
    (user/eval-until-move
     '(((user/use-helm-gtags)
        (call-interactively 'helm-gtags-select-path))
       ((boundp 'projectile-find-file)
        (call-interactively 'projectile-find-file))))))


(defun user/tag-pop ()
  "Return to the previous point before jump."
  (interactive)
  (unless (and (user/use-rtags)
               (rtags-location-stack-back))
    (user/eval-until-move
     '(((user/use-helm-gtags)
        (call-interactively 'helm-gtags-pop-stack))
       (t (pop-global-mark))))))


(defun user/tags-try-enable ()
  "Enable tags if any of the tag backends is located."
  (when (or (user/use-rtags)
            (user/use-helm-gtags)
            (user/use-semantic))
    (when (feature-p 'rtags)
      (ignore-errors
        (rtags-start-process-unless-running)))

    ;;; (Bindings) ;;;
    (user/bind-key-local :code :update-index 'user/tag-update-index)
    (user/bind-key-local :nav :functions/toc 'user/tag-toc)
    (user/bind-key-global :nav :follow-symbol 'user/tag-follow)
    (user/bind-key-local :nav :find-symbol 'user/tag-find)
    (user/bind-key-local :nav :references 'user/tag-references-at-point)
    (user/bind-key-local :nav :find-virtuals 'user/tag-find-virtuals)
    (user/bind-key-local :nav :find-references 'user/tag-find-references)
    (user/bind-key-local :code :insert-dependency 'user/tag-insert-dependency-from-point)
    (user/bind-key-local :basic :open-file-context 'user/tag-find-file)
    (user/bind-key-local :nav :file-dependencies 'user/tag-file-dependencies)
    (user/bind-key-local :nav :go-back 'user/tag-pop)))

(use-package rtags
  :if (executable-find "llvm-config")
  :defer
  :config
  (use-package ac-rtags)
  (use-package company-rtags)
  (use-package flycheck-rtags)
  (use-package helm-rtags)

  (after-load 'tramp
    (validate-setq
     ;; Enable tramp after it has been loaded.
     rtags-tramp-enabled t)
    ;; This function is causing tramp to lock up waiting for a
    ;; prompt so disable it for now.
    (defun rtags-update-current-project ())))

(when (feature-p 'helm)
  (use-package helm-etags-plus
    :defer))


(provide 'utilities/tags)
;;; tags.el ends here
