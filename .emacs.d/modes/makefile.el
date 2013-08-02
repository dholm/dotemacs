;;; makefile --- initializes Makefile modes
;;; Commentary:
;;; Code:

(defun user/makefile-mode-cedet-hook ()
  "CEDET hook for makefiles."
  (user/cedet-hook)
  (require 'semantic/bovine/make))


;; Sets some decent defaults for makefile-mode
(defun user/makefile-mode-hook ()
  "Initialize makefile mode."
  ;; Load CEDET for makefiles
  (after-load 'cedet
    (user/makefile-mode-cedet-hook))
  ;; Use tabs for indent
  (setq indent-tabs-mode t)
  ;; Separate camel-case into separate words
  (subword-mode t))

(add-hook 'makefile-mode-hook 'user/makefile-mode-hook)


(provide 'modes/makefile)
;;; makefile.el ends here
