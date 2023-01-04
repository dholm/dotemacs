;;; prologue.el --- Emacs init prologue
;;; Commentary:
;;; Code:

(eval-when-compile
  (defconst *user-emacs-lisp-directory*
    (expand-file-name "lisp" user-emacs-directory)
    "Path to user Emacs Lisp directory.")

  ;; Allow Emacs to find configuration files.
  (add-to-list 'load-path *user-emacs-lisp-directory*))


;; Bring in constants used throughout initialization.
(require 'init-constants)
;; Initialize basic Emacs functionality.
(require 'init-emacs)


;; Load Emacs utilities.
(require 'lib/list)
(require 'lib/string)
(require 'lib/with)
(require 'lib/utils)

;; Set up package management.
(require 'lib/packaging)

;; Install benchmark-init if present.
(use-package benchmark-init
  :pin "MELPA"
  :ensure t)

;; Helper functions for bootstrapping Emacs.
(require 'lib/bootstrap)

;; Helper functions for other applications.
(require 'lib/apps)

;; Load Emacs user experience settings.
(require 'init-bindings)
(require 'init-ux)

;;; prologue.el ends here
