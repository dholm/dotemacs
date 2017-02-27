;;; projectile.el --- Projectile project management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :defer
  :init
  ;;; (Bindings) ;;;
  (user/bind-key-global :basic :open-file-context 'projectile-find-file)
  :config
  (validate-setq
   ;; Projectile bookmarks.
   projectile-known-projects-file (path-join *user-data-directory*
                                             "projectile-bookmarks.eld")
   ;; Projectile cache store.
   projectile-cache-file (path-join *user-cache-directory* "projectile")
   ;; Use default completion that will usually be provided by Helm.
   projectile-completion-system 'default)

  (after-load 'smart-mode-line
    (validate-setq
     ;; Enable in smart mode line.
     sml/use-projectile-p 'after-prefixes))

  (with-executable 'ctags-exuberant
    (validate-setq
     ;; Default to exuberant ctags.
     projectile-tags-command "ctags-exuberant -Re %s")))


(provide 'utilities/projectile)
;;; projectile.el ends here
