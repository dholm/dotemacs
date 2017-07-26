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


;; Disable collection of benchmark data.
(when (featurep 'benchmark-init)
  (benchmark-init/deactivate))

;; Add potential new file name handlers set during init and then clear
;; it again.
(delete-dups
 (nconc *user--file-name-handler-alist-original* file-name-handler-alist))
(validate-setq
 ;; Don't invoke any file name handlers during init.
 file-name-handler-alist nil)


;;; epilogue.el ends here
