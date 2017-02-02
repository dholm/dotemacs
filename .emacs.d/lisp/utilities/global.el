;;; global.el --- Set up GNU Global support.
;;; Commentary:
;;; Code:

(defun user--helm-gtags-mode-hook ()
  "Mode hook for helm-gtags."
  ;; Automatically update GNU Global database if it exists.
  (when (user/gnu-global-tags-p (buffer-file-name))
    (setq
     helm-gtags-auto-update (not (and (boundp 'ggtags-update-on-save)
                                      ggtags-update-on-save))
     helm-gtags-tag-location (user/gnu-global-tags-location
                              (buffer-file-name)))))


(defun user--ggtags-mode-hook ()
  "Mode hook for ggtags."
  (when (user/gnu-global-tags-p (buffer-file-name))
    (setq
     ggtags-update-on-save (not (and (boundp 'helm-gtags-auto-update)
                                     helm-gtags-auto-update))))

  (when (tramp-tramp-file-p (buffer-file-name (current-buffer)))
    ;; Disable eldoc in tramp buffers.
    (remove-function (local 'eldoc-documentation-function) 'ggtags-eldoc-function)))


(defun user/gnu-global-tags-location (path)
  "Get the location of Global's database from PATH, if it exists."
  (with-project-root proj-root path
    (when (file-exists-p (path-join proj-root "GTAGS"))
      proj-root)))


(defun user/gnu-global-tags-p (path)
  "Check if a GNU Global tag database exists for project in PATH."
  (when (user/gnu-global-tags-location path)
    t))


(defun user/gnu-global-enable ()
  "Activate GNU Global in current major mode."
  (with-feature 'helm-gtags
    (after-load 'diminish
      (diminish 'helm-gtags-mode))
    (helm-gtags-mode t))

  (with-feature 'ggtags
    (after-load 'diminish
      (diminish 'ggtags-mode))
    (ggtags-mode t)

    (unless (and (boundp 'imenu-create-index-function)
                 imenu-create-index-function)
      (setq-local
       ;; Use ggtags to generate imenu.
       imenu-create-index-function #'ggtags-build-imenu-index))

    (setq-local
     ;; Use as source for `hippie-exp'.
     hippie-expand-try-functions-list
     (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list)))

  (after-load 'semantic
    (with-feature 'semantic/db-global
      ;; Enable semantic GNU/GLOBAL database.
      (semanticdb-enable-gnu-global-in-buffer t)))

  ;; Register as auto-completion source.
  (add-ac-sources 'ac-source-gtags)

  (user/tags-try-enable))


(defsubst user/gnu-global-query-label ()
  "Query for GNU GLOBAL label."
  (let ((labels '("default" "native" "user" "ctags" "pygments")))
    (completing-read "GTAGSLABEL(Default: default): " labels nil t nil nil
                     "default")))


(defun user/gnu-global-create/update ()
  "Create or update GNU GLOBAL database at current project root."
  (interactive)
  (with-executable 'global
    (with-project-root project-root nil
      (cond
       ((require 'helm-gtags nil :noerror)
        (helm-gtags-create-tags project-root (user/gnu-global-query-label)))
       ((require 'ggtags nil :noerror)
        (ggtags-create-tags project-root))
       ((require 'cedet-global nil :noerror)
        (cedet-gnu-global-create/update-database project-root))))))


(defun user--helm-gtags-config ()
  "Initialize helm-gtags."
  (validate-setq
   ;; Don't care about case when searching tags.
   helm-gtags-ignore-case t
   ;; When navigating open buffers in r/w mode.
   helm-gtags-read-only nil
   ;; Use input at cursor.
   helm-gtags-use-input-at-cursor t
   ;; Pulse at point after jump.
   helm-gtags-pulse-at-cursor t)

  ;;; (Hooks) ;;;
  (add-hook 'helm-gtags-mode-hook 'user--helm-gtags-mode-hook))


(defun user--ggtags-config ()
  "Initialize ggtags."
  (validate-setq
   ;; Never use global to highlight tags.
   ggtags-highlight-tag nil)

  ;;; (Hooks) ;;;
  (add-hook 'ggtags-mode-hook 'user--ggtags-mode-hook))


(defun user--global-config ()
  "Initialize GNU Global support."
  (add-to-list
   ;; Don't invoke debugger if global can't find its tags.
   'debug-ignored-errors "global: GTAGS not found")

  ;;; (Packages) ;;;
  (use-package helm-gtags
    :defer t
    :config (user--helm-gtags-config))
  (use-package ggtags
    :defer t
    :config (user--ggtags-config)))

(with-executable 'global
  (user--global-config))


(provide 'utilities/global)
;;; global.el ends here
