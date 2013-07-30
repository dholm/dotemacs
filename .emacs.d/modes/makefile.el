;;; makefile --- initializes Makefile modes
;;; Commentary:
;;; Code:

(defun dholm/makefile-mode-cedet-hook ()
  "CEDET hook for makefiles."
  (dholm/cedet-hook)
  (require 'semantic/bovine/make))


;; Sets some decent defaults for makefile-mode
(defun dholm/makefile-mode-hook ()
  "Initialize makefile mode."
  ;; Load CEDET for makefiles
  (after-load 'cedet
    (dholm/makefile-mode-cedet-hook))
  ;; Use tabs for indent
  (setq indent-tabs-mode t)
  ;; Separate camel-case into separate words
  (subword-mode t))

(add-hook 'makefile-mode-hook 'dholm/makefile-mode-hook)


(provide 'modes/makefile)
;;; makefile.el ends here
