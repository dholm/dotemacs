;;; rfc.el --- RFC mode support
;;; Commentary:
;;; Code:

(defun user/rfc-mode-hook ()
  "RFC mode hook.")


(defun user/irfc-init ()
  "Initialize irfc."
  (setq-default
   ;; Put RFCs in user cache directory.
   irfc-directory (path-join *user-cache-directory* "rfcs")
   ;; Load irfc mode automatically.
   irfc-assoc-mode t)

  ;; Create cache directory for irfc.
  (make-directory irfc-directory t)

  (add-hook 'irfc-mode-hook 'user/rfc-mode-hook)
  (add-auto-mode 'irfc-mode "/rfc[0-9]+\\.txt\\'"))


(req-package irfc
  :config (user/irfc-init))


(provide 'modes/rfc)
;;; rfc.el ends here
