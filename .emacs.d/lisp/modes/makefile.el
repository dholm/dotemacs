;;; makefile.el --- Initializes Makefile mode
;;; Commentary:
;;; Code:

(defun user/makefile-mode-cedet-hook ()
  "CEDET hook for makefiles."
  (with-feature 'semantic/bovine/make
    (user/cedet-hook)))


;; Sets some decent defaults for makefile-mode
(defun user/makefile-mode-hook ()
  "Initialize makefile mode."
  ;; Load CEDET for makefiles
  (user/makefile-mode-cedet-hook)
  ;; Use tabs for indent
  (setq indent-tabs-mode t)
  ;; Separate camel-case into separate words
  (subword-mode t))


(defun user/makefile-mode-init ()
  "Initialize makefile mode."
  (add-hook 'makefile-mode-hook 'user/makefile-mode-hook)

  (add-auto-mode 'makefile-mode "\\.mak$"))

(user/makefile-mode-init)


(provide 'modes/makefile)
;;; makefile.el ends here
