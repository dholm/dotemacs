;;; prologue.el --- Emacs init prologue
;;; Commentary:
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `req-package'.
(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))

(eval-when-compile
  (defconst *user-emacs-lisp-directory*
    (expand-file-name "lisp" user-emacs-directory)
    "Path to user Emacs Lisp directory.")

  ;; Allow Emacs to find configuration files.
  (add-to-list 'load-path *user-emacs-lisp-directory*)

  ;; Load req-package.
  (require 'req-package))


;; Bring in constants used throughout initialization.
(require 'init-constants)


;; Load Emacs utilities.
(require 'lib/list)
(require 'lib/string)
(require 'lib/with)
(require 'lib/utils)


;; Install benchmark-init if present.
(let ((benchmark-init.el (path-join *user-el-get-directory*
                                    "benchmark-init" "benchmark-init.el")))
  (when (file-exists-p benchmark-init.el)
    (load benchmark-init.el)))


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
