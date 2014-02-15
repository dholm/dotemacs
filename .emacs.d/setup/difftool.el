;;; difftool.el --- Emacs as a difftool
;;; Commentary:
;;; Usage:

;; emacs -Q --load difftool.el

;;; Code:

(eval-and-compile
  ;; Load Emacs init prologue.
  (load (expand-file-name "prologue.el" user-emacs-directory))

  (require 'ediff))


;; Load VCS support.
(require 'init-vcs)


(let ((local (getenv "LOCAL"))
      (remote (getenv "REMOTE")))
  (ediff local remote))


;; Load Emacs init epilogue.
(load (expand-file-name "epilogue.el" user-emacs-directory))
;;; difftool.el ends here
