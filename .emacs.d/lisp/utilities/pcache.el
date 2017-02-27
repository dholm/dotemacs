;;; pcache.el --- Configure Emacs persistent cache -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pcache
  :defer
  :config
  (validate-setq
   pcache-directory (path-join *user-cache-directory* "pcache")))


(provide 'utilities/pcache)
;;; pcache.el ends here
