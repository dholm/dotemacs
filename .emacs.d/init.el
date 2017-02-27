;;; init.el --- Emacs main initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Bring in package.
(setq-default
  package--init-file-ensured t
  package-enable-at-startup nil)
(require 'package)

(eval-and-compile
  ;; Load Emacs init prologue.
  (load (expand-file-name "prologue.el" user-emacs-directory)))

;; Set up package management.
(require 'lib/packaging)
;; Load utilities.
(require 'lib/utils)
(require 'lib/hash-tables)
(require 'lib/bootstrap)
(require 'lib/apps)
(require 'lib/introspection)


;; Load full configuration.
(require 'init-utilities)
(require 'init-vcs)
(require 'init-modes)
(require 'init-apps)


;; Load Emacs init epilogue.
(load (expand-file-name "epilogue.el" user-emacs-directory))
;;; init.el ends here
