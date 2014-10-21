;;; global.el --- Set up GNU Global support.
;;; Commentary:
;;; Code:

(defun user/helm-gtags-mode-hook ()
  "Mode hook for helm-gtags."
  (setq-local
   helm-gtags-tag-location (user/tags-location))

  ;; Automatically update GNU Global database if it exists.
  (when (user/gnu-global-tags-p)
    (setq-default
     helm-gtags-auto-update t
     helm-gtags-tag-location (user/tags-location)))

  ;;; (Bindings) ;;;
  (user/bind-key-local :nav :find-symbol 'helm-gtags-select)
  (user/bind-key-local :nav :go-back 'helm-gtags-pop-stack)
  (user/bind-key-local :nav :references 'helm-gtags-find-rtag))


(defun user/gnu-global-tags-p (&optional path)
  "Non-nil if tags exist for PATH or current buffer if nil."
  (file-exists-p (path-join (user/tags-location path) "GTAGS")))


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

    (setq-local
     ;; Enable Eldoc support.
     eldoc-documentation-function #'ggtags-eldoc-function)))


(defun user/gnu-global-create/update ()
  "Create or update GNU GLOBAL database at current project root."
  (interactive)
  (with-executable 'global
    (with-project-root project-root nil
      (make-directory (user/tags-location) t)
      (cond
       ((require 'cedet-global nil :noerror)
        (cedet-gnu-global-create/update-database project-root))
       ((require 'ggtags nil :noerror)
        (ggtags-create-tags project-root))))))


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


(defun user/ggtags-init ()
  "Initialize ggtags."
  (defadvice ggtags-global-start
      (around ggtags-global-start-real-root (command &optional directory) activate)
    (let ((directory (user/tags-location)))
      ad-do-it)))


(defun user/global-init ()
  "Initialize GNU Global support."
  (after-load 'cedet-global
    (defadvice cedet-gnu-global-call
        (around cedet-gnu-global-db-root (flags) activate)
      (with-project-root project-root nil
        (let ((old-gtagsroot (getenv "GTAGSROOT"))
              (old-gtagsdbpath (getenv "GTAGSDBPATH")))
          (setenv "GTAGSROOT" project-root)
          (setenv "GTAGSDBPATH" (user/tags-location))
          (prog1
              ad-do-it
            (setenv "GTAGSROOT" old-gtagsroot)
            (setenv "GTAGSDBPATH" old-gtagsdbpath)))))
    (defadvice cedet-gnu-global-gtags-call
        (around cedet-gnu-global-db-root (flags) activate)
      (let ((flags (append flags (list (user/tags-location)))))
        (message "Call with %s" flags)
        ad-do-it)))

  ;;; (Packages) ;;;
  (require-package '(:name helm-gtags :after (user/helm-gtags-init)))
  (require-package '(:name ggtags :after (user/ggtags-init))))

(with-executable 'global
  (user/global-init))


(provide 'utilities/global)
;;; global.el ends here
