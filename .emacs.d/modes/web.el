(setq mumamo-background-colors nil)

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
                   '(setq mumamo-per-buffer-local-vars
                          (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

