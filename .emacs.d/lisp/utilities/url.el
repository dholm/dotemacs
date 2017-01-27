;;; url.el --- Emacs URL package
;;; Commentary:
;;; Code:

(defconst *user-url-cache-directory* (path-join *user-cache-directory* "url"))

(defun user--url-config ()
  "Initialize url package."
  (setq-default
   ;; Set up cache directory.
   url-configuration-directory *user-url-cache-directory*
   url-cookie-file (path-join *user-url-cache-directory* "cookies")
   url-history-file (path-join *user-url-cache-directory* "history")
   ;; Automatically cache all documents.
   url-automatic-caching t))

(user--url-config)


(provide 'utilities/url)
;;; url.el ends here
