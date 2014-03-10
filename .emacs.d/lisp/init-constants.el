;;; init-constants.el --- Set up constants required during initialization
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/env)
(require 'lib/pkg-config)


;;; (Directories) ;;;
(defconst *user-home-directory*
  (getenv-or "HOME" (concat (expand-file-name "~") "/"))
  "Path to user home directory.")
(defconst *user-data-directory*
  (getenv-or "XDG_DATA_HOME"
             (path-join *user-home-directory* ".local" "share" "emacs"))
  "Path to user's local data store.")
(defconst *user-cache-directory*
  (getenv-or "XDG_CACHE_HOME"
             (path-join *user-home-directory* ".cache" "emacs"))
  "Path to user's local cache store.")
(defconst *user-documents-directory*
  (path-join *user-home-directory* "Documents")
  "Path to user's documents directory.")

(defconst *user-el-get-directory*
  (path-join user-emacs-directory "el-get")
  "Path to user's el-get store.")
(defconst *user-local-init*
  (path-join *user-home-directory* ".emacs.local.el")
  "Path to user's machine-local configuration file.")


(provide 'init-constants)
;;; init-constants.el ends here
