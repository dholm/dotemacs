
(setq load-path (cons "~/.emacs.d" load-path))
(setq load-path (cons "~/.emacs.d/vendor" load-path))

(load "emacs")
(load "theme")
(load "utilities")
(load "conventions")
(load "bindings")

;; If ~/.emacs.local is available load it as the last file so that it is
;; possible to add local settings and overrides.
(if (file-readable-p (expand-file-name "~/.emacs.local"))
    (load-file (expand-file-name "~/.emacs.local"))
  nil)
