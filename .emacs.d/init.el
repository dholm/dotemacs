;;; init.el --- Emacs main initialization
;;; Commentary:
;;; Code:

;; Bootstrap Emacs and load benchmark
(add-to-list 'load-path user-emacs-directory)

(require 'lib/path)
(add-to-list 'exec-path (path-join user-emacs-directory "bin"))

(require 'lib/benchmark)
(benchmark/install)

;; Bring in constants used throughout initialization
(require 'init-constants)

;; Set up package management
(require 'lib/packaging)
;; Load utilities
(require 'lib/utils)
(require 'lib/hash-tables)
(require 'lib/pkg-config)


;; Load configuration
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


;; Synchronize all registered packages
(user/sync-packages)


;; Load custom after all packages have been synced
(when (file-exists-p *user-custom-file*)
  (load *user-custom-file*))
;;; init.el ends here
