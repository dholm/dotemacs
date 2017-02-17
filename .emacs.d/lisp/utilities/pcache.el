;;; pcache.el --- Configure Emacs persistent cache
;;; Commentary:
;;; Code:

(use-package pcache
  :defer t
  :config
  (validate-setq
   pcache-directory (path-join *user-cache-directory* "pcache")))


(provide 'utilities/pcache)
;;; pcache.el ends here
