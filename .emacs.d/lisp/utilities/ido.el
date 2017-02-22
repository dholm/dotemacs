;;; ido.el --- interactively do things
;;; Commentary:
;;; Code:

(defun user--ido-mode-hook ()
  "Mode hook for ido."
  (when (feature-p 'flx)
    (flx-ido-mode t))
  (when (feature-p 'ido-vertical-mode)
    (ido-vertical-mode t)))

(use-package ido
  :defer t
  :config
  (validate-setq
   ;; Enable fuzzy matching
   ido-enable-flex-matching t
   ;; Remember buffers that have been open
   ido-use-virtual-buffers t
   ;; Allow the same buffer to be opened in different windows
   ido-default-buffer-method 'selected-window)

  (use-package flx
    :config
    (validate-setq
     ;; Flex has its own highlights.
     ido-use-faces nil))
  (use-package ido-vertical-mode))


(provide 'utilities/ido)
;;; ido.el ends here
