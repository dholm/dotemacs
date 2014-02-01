;;; url.el --- Emacs URL package
;;; Commentary:
;;; Code:

(defconst *user-url-directory* (path-join *user-cache-directory* "url"))

(setq
 url-cookie-file (path-join *user-url-directory* "cookies")
 url-history-file (path-join *user-url-directory* "history"))


(provide 'utilities/url)
;;; url.el ends here
