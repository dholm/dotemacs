;;; makefile.el --- Initializes Makefile mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--makefile-mode-cedet-hook ()
  "CEDET hook for makefiles."
  (with-feature 'semantic/bovine/make
    (user--cedet-hook)))


;; Sets some decent defaults for makefile-mode
(defun user--makefile-mode-hook ()
  "Initialize makefile mode."
  ;; Load CEDET for makefiles.
  (user--makefile-mode-cedet-hook)
  (setq
   ;; Use tabs for indent.
   indent-tabs-mode t)
  ;; Disable whitespace mode settings that don't make sense in makefiles.
  (user/whitespace-disable-style '(indentation space-after-tab))
  ;; Separate camel-case into separate words.
  (subword-mode t)
  ;; Support for documentation in Doxygen format.
  (with-feature 'doxymacs
    (doxymacs-mode t))

  (with-feature 'makefile-executor
    (makefile-executor-mode t)))

(use-package make-mode
  :defer
  :mode ("\.\(mak\|mif\|wat\)$" . makefile-mode)
  :init
  (add-hook 'makefile-mode-hook 'user--makefile-mode-hook)
  :config
  (use-package makefile-executor))


(provide 'modes/makefile)
;;; makefile.el ends here
