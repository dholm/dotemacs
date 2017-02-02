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
  (after-load 'ido
    (validate-setq
     ;; Flex has its own highlights.
     ido-use-faces nil)))


(defun user--ido-config ()
  "Initialize ido."
  (validate-setq
   ;; Enable fuzzy matching
   ido-enable-flex-matching t
   ;; Remember buffers that have been open
   ido-use-virtual-buffers t
   ;; Allow the same buffer to be opened in different windows
   ido-default-buffer-method 'selected-window))

(use-package ido
  :defer t
  :config (user--ido-config))

(use-package flx
  :after ido
  :config (user--flx-config))

(use-package ido-vertical-mode
  :after ido)


(provide 'utilities/ido)
;;; ido.el ends here
