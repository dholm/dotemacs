;;; init.el --- Emacs main initialization
;;; Commentary:
;;; Code:

;; Bootstrap Emacs.
(add-to-list 'load-path user-emacs-directory)

(require 'lib/path)
(add-to-list 'exec-path (path-join user-emacs-directory "bin"))


;; Bring in constants used throughout initialization.
(require 'lib/pkg-config)
(require 'init-constants)


;; Install benchmark-init if present.
(let ((benchmark-init-path (path-join *user-el-get-directory* "benchmark-init")))
  (when (file-exists-p benchmark-init-path)
    (add-to-list 'load-path benchmark-init-path)
    (require 'benchmark-init)
    (benchmark-init/install)))


;; Set up package management.
(require 'lib/packaging)
;; Load utilities.
(require 'lib/utils)
(require 'lib/hash-tables)
(require 'lib/bootstrap)
(require 'lib/apps)


;; Load configuration.
(require 'init-emacs)
(require 'init-bindings)
(require 'init-ux)
(require 'init-utilities)
(require 'init-vcs)
(require 'init-modes)
(require 'init-apps)


;; Load user's machine-local configuration file, if available.
(when (file-exists-p *user-local-init*)
  (load *user-local-init*))


;; Synchronize all registered packages.
(user/sync-packages)


;; Load custom after all packages have been synced.
(when (file-exists-p *user-custom-file*)
  (load *user-custom-file*))
;;; init.el ends here
