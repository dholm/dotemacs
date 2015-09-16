;;; global.el --- Set up GNU Global support.
;;; Commentary:
;;; Code:

(defun user/helm-gtags-mode-hook ()
  "Mode hook for helm-gtags."
  ;; Automatically update GNU Global database if it exists.
  (when (user/gnu-global-tags-p (buffer-file-name))
    (setq-default
     helm-gtags-auto-update t
     helm-gtags-tag-location (user/gnu-global-tags-location
                              (buffer-file-name)))))


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
     ;; Enable Eldoc support.
     eldoc-documentation-function #'ggtags-eldoc-function))

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


(defun user/helm-gtags-init ()
  "Initialize helm-gtags."
  (setq-default
   ;; Don't care about case when searching tags.
   helm-gtags-ignore-case t
   ;; When navigating open buffers in r/w mode.
   helm-gtags-read-only nil
   ;; Use input at cursor.
   helm-gtags-use-input-at-cursor t
   ;; Pulse at point after jump.
   helm-gtags-pulse-at-cursor t)

  (add-hook 'helm-gtags-mode-hook 'user/helm-gtags-mode-hook))


(defun user/global-init ()
  "Initialize GNU Global support."
  (add-to-list
   ;; Don't invoke debugger if global can't find its tags.
   'debug-ignored-errors "global: GTAGS not found")

  ;;; (Packages) ;;;
  (require-package '(:name helm-gtags :after (user/helm-gtags-init)))
  (require-package '(:name ggtags)))

(with-executable 'global
  (user/global-init))


(provide 'utilities/global)
;;; global.el ends here
