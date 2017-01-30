;;; ido.el --- interactively do things
;;; Commentary:
;;; Code:

(defun user--ido-mode-hook ()
  "Mode hook for ido."
  (when (feature-p 'flx)
    (flx-ido-mode t))
  (when (feature-p 'ido-vertical-mode)
    (ido-vertical-mode t)))


(defun user--flx-config ()
  "Initialize FLX."
  (setq-default
   ;; Flex has its own highlights.
   ido-use-faces nil))


(defun user--ido-config ()
  "Initialize ido."
  (setq-default
   ;; Enable fuzzy matching
   ido-enable-flex-matching t
   ;; Remember buffers that have been open
   ido-use-virtual-buffers t
   ;; Allow the same buffer to be opened in different windows
   ido-default-buffer-method 'selected-window)

  ;;; (Packages) ;;;
  (use-package flx
    :ensure t
    :config (user--flx-config))
  (use-package ido-vertical-mode
    :ensure t))

(user--ido-config)


(provide 'utilities/ido)
;;; ido.el ends here
