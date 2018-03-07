;;; cache.el --- Configure Emacs persistent caches -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pcache
  :config
  (validate-setq
   pcache-directory (path-join *user-cache-directory* "pcache")))


(provide 'ux/cache)
;;; cache.el ends here
