;;; password-cache.el --- Emacs password cache
;;; Commentary:
;;; Code:

(use-package password-cache
  :defer t
  :config
  (validate-setq
   ;; Keep password cache longer.
   password-cache-expiry 3600))


(provide 'utilities/password-cache)
;;; password-cache.el ends here
