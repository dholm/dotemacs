;;; prologue.el --- Emacs init prologue
;;; Commentary:
;;; Code:

(eval-when-compile
  (defconst *user-emacs-lisp-directory*
    (expand-file-name "lisp" user-emacs-directory)
    "Path to user Emacs Lisp directory.")

  ;; Allow Emacs to find configuration files.
  (add-to-list 'load-path *user-emacs-lisp-directory*))


;; Allow Emacs to use binaries from bin.
(add-to-list 'exec-path (expand-file-name "bin" user-emacs-directory))


;; Bring in constants used throughout initialization.
(require 'init-constants)


;; Load Emacs utilities.
(require 'lib/utils)


;; Install benchmark-init if present.
(let ((benchmark-init-path (path-join *user-el-get-directory* "benchmark-init")))
  (add-to-list 'load-path benchmark-init-path)
  (with-feature 'benchmark-init
    (benchmark-init/install)))


;; Set up package management.
(require 'lib/packaging)

;; Helper functions for bootstrapping Emacs.
(require 'lib/bootstrap)

;; Helper functions for other applications.
(require 'lib/apps)

;; Initialize basic Emacs functionality.
(require 'init-emacs)
(require 'init-bindings)
(require 'init-ux)

;;; prologue.el ends here
