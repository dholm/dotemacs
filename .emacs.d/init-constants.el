;;; init-constants.el --- Set up constants required during initialization
;;; Commentary:
;;; Code:

(require 'lib/env)
(require 'lib/path)

(defconst *user-home-directory* (getenv-or "HOME"
                                           (concat (expand-file-name "~") "/")))
(defconst *user-data-directory* (getenv-or "XDG_DATA_HOME"
                                           (path-join *user-home-directory* ".local" "share" "emacs")))
(defconst *user-cache-directory* (getenv-or "XDG_CACHE_HOME"
                                            (path-join *user-home-directory* ".cache" "emacs")))
(defconst *user-el-get-directory* (path-join user-emacs-directory "el-get"))
(defconst *user-custom-file* (path-join *user-data-directory* "custom.el"))


(provide 'init-constants)
;;; init-constants.el ends here
