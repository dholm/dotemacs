;;; rfc.el --- RFC mode support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package irfc
  :defer
  :mode ("/rfc[0-9]+\\.txt\\'" . irfc-mode)
  :config
  ;; Create cache directory for irfc.
  (make-directory irfc-directory t)

  (validate-setq
   ;; Put RFCs in user cache directory.
   irfc-directory (path-join *user-cache-directory* "rfcs")
   ;; Load irfc mode automatically.
   irfc-assoc-mode t))


(provide 'modes/rfc)
;;; rfc.el ends here
