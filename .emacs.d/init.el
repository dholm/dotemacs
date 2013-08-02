;;; init --- Emacs main initialization
;;; Commentary:
;;; Code:

;; Bootstrap Emacs and load benchmarking
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'exec-path (concat user-emacs-directory "bin"))
(require 'lib/benchmarking)


;; Set up global constants
(require 'lib/path)
(require 'lib/env)
(defconst *user-home-directory* (concat (expand-file-name "~") "/"))
(defconst *user-data-directory* (getenv-or "XDG_DATA_HOME"
                                           (path-join *user-home-directory* ".local" "share" "emacs")))
(defconst *user-cache-directory* (getenv-or "XDG_CACHE_HOME"
                                            (path-join *user-home-directory* ".cache" "emacs")))
(defconst *user-el-get-directory* (path-join user-emacs-directory "el-get"))
(defconst *user-custom-file* (path-join *user-data-directory* "custom.el"))


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


;; If ~/.emacs.local is available load it as the last file so that it is
;; possible to add local settings and overrides.
(setq user-local-init (path-join *user-home-directory* ".emacs.local"))
(when (file-exists-p user-local-init)
  (load user-local-init))


;; Synchronize all registered packages
(user/sync-packages)


;; Load custom after all packages have been synced
(when (file-exists-p custom-file)
  (load custom-file))
;;; init.el ends here
