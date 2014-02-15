;;; epilogue.el --- Emacs init epilogue
;;; Commentary:
;;; Code:

(eval-when-compile
  (load (expand-file-name "prologue.el" user-emacs-directory))
  (require 'init-emacs))

;; Load user's machine-local configuration file, if available.
(when (file-exists-p *user-local-init*)
  (load *user-local-init*))


;; Synchronize all registered packages.
(user/sync-packages)


;; Load custom after all packages have been synced.
(when (file-exists-p *user-custom-file*)
  (load *user-custom-file*))


;;; epilogue.el ends here
