
(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/vendor" load-path))

(push "~/.emacs.d/vendor/bin" exec-path)

(load "emacs")
(load "theme")
(load "utilities")
(load "conventions")
(load "bindings")

;; Load language-specific configurations
(load "lisp")
(load "python")


;; If ~/.emacs.local is available load it as the last file so that it is
;; possible to add local settings and overrides.
(if (file-readable-p (expand-file-name "~/.emacs.local"))
    (load-file (expand-file-name "~/.emacs.local"))
  nil)
