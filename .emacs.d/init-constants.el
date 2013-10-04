;;; init-constants.el --- Set up constants required during initialization
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/env)


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
(defconst *user-el-get-directory*
  (path-join user-emacs-directory "el-get")
  "Path to user's el-get store.")
(defconst *user-local-init*
  (path-join *user-home-directory* ".emacs.local.el")
  "Path to user's machine-local configuration file.")


;;; (Binaries) ;;;
(defconst *has-aspell* (executable-find "aspell"))
(defconst *has-global* (executable-find "global"))
(defconst *has-cscope* (executable-find "cscope"))
(defconst *has-ctags* (executable-find "ctags"))
(defconst *has-idutils* (executable-find "mkid"))

(defconst *has-python* (executable-find "python"))
(defconst *has-scala* (executable-find "scala"))
(when *has-scala*
  (defconst *has-sbt* (executable-find "sbt")))
(defconst *has-clang* (executable-find "clang"))
(defconst *has-ruby* (executable-find "ruby"))
(defconst *has-go* (executable-find "go"))
(defconst *has-lua* (executable-find "lua"))
(defconst *has-perl* (executable-find "perl"))
(defconst *has-php* (executable-find "php"))
(defconst *has-octave* (executable-find "octave"))
(defconst *has-gdb* (executable-find "gdb"))
(defconst *has-sbcl* (executable-find "sbcl"))
(defconst *has-lisp* (executable-find "lisp"))
(defconst *has-clisp* (executable-find "clisp"))
(defconst *has-ghc* (executable-find "ghc"))
(defconst *has-cmake* (executable-find "cmake"))

(defconst *has-git* (executable-find "git"))
(defconst *has-cleartool* (and (executable-find "cleartool")
                             ;; Verify that license is valid
                             (eq (call-process-shell-command "cleartool" nil nil nil "quit") 0)))

(defconst *has-libpurple*
  (and
   (pkg-config-has-p "libxml-2.0")
   (pkg-config-has-p "purple")))
(defconst *has-w3m* (executable-find "w3m"))


(provide 'init-constants)
;;; init-constants.el ends here
