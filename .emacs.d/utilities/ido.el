;;; ido.el --- interactively do things
;;; Commentary:
;;; Code:

(defun dholm/ido-init ()
  "Initialize ido."
  (setq-default
   ;; Enable fuzzy matching
   ido-enable-flex-matching t
   ;; Remember buffers that have been open
   ido-use-virtual-buffers t
   ;; Allow the same buffer to be opened in different windows
   ido-default-buffer-method 'selected-window))

(dholm/ido-init)


(provide 'utilities/ido)
;;; ido.el ends here
