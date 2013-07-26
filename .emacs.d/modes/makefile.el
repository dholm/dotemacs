;;; makefile --- initializes Makefile modes
;;; Commentary:
;;; Code:

(defun dholm/makefile-mode-cedet-hook ()
  (dholm/cedet-hook)
  (require 'semantic/bovine/make))


;; Sets some decent defaults for makefile-mode
(defun dholm/makefile-mode-hook ()
  "Initialize makefile mode."
  ;; Load CEDET for makefiles
  (dholm/makefile-mode-cedet-hook)
  ;; Use tabs for indent
  (setq indent-tabs-mode t)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode)
  ;; Separate camel-case into separate words
  (subword-mode t)
  (add-hook 'before-save-hook
            ;; Delete trailing whitespace on save
            'delete-trailing-whitespace nil t))


(add-hook 'makefile-mode-hook 'dholm/makefile-mode-hook)


(provide 'modes/makefile)
;;; makefile.el ends here
