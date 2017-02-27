;;; net.el --- Initialize Emacs networking -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lib/path)
(require 'lib/utils)


(defconst *user-url-cache-directory*
  (path-join *user-cache-directory* "url")
  "Path to user's url data store.")
(defconst *user-nsm-data-directory*
  (path-join *user-data-directory* "nsm")
  "Path to user's Wanderlust data store.")


(after-load 'url
  (setq
   ;; Set up cache directory.
   url-configuration-directory *user-url-cache-directory*
   url-cookie-file (path-join *user-url-cache-directory* "cookies")
   url-history-file (path-join *user-url-cache-directory* "history")
   ;; Automatically cache all documents.
   url-automatic-caching t))

(make-directory *user-nsm-data-directory* t)
(after-load 'nsm
  (setq
   ;; Location of security manager settings.
   nsm-settings-file
   (path-join *user-nsm-data-directory* "network-security.data")))


(provide 'lib/net)
;;; net.el ends here
