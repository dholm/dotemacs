;;; presentation.el --- Emacs presentation mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun user--presentation-on-hook ()
  "Hook executed when enabling presentation mode."
  ;; Disable Helm.
  (helm-mode -1))

(defun user--presentation-off-hook ()
  "Hook executed when disabling presentation mode."
  ;; Enable Helm.
  (helm-mode 1))

(use-package presentation
  :hook ((presentation-on-hook . user--presentation-on-hook)
         (presentation-off-hook . user--presentation-off-hook))
  :bind-wrap ((:key :util :presentation) . presentation-mode))


(provide 'utilities/presentation)
;;; presentation.el ends here
