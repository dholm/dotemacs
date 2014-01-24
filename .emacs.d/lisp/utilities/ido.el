;;; ido.el --- interactively do things
;;; Commentary:
;;; Code:

(defun user/ido-init ()
  "Initialize ido."
  (setq-default
   ;; Enable fuzzy matching
   ido-enable-flex-matching t
   ;; Remember buffers that have been open
   ido-use-virtual-buffers t
   ;; Allow the same buffer to be opened in different windows
   ido-default-buffer-method 'selected-window))

(user/ido-init)

(defun user/flx-init ()
  "Initialize FLX."
  (flx-ido-mode t)
  (setq-default ido-use-faces nil))

(defun user/ido-vertical-mode-init ()
  "Initialize IDO vertical mode."
  (ido-vertical-mode))

(require-package '(:name flx :after (user/flx-init)))
(require-package '(:name ido-vertical-mode :after (user/ido-vertical-mode-init)))


(provide 'utilities/ido)
;;; ido.el ends here
