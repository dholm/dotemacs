;;; w3m.el --- w3m integration
;;; Commentary:
;;; Code:

(defconst *user-w3m-data-directory*
  (path-join *user-data-directory* "w3m")
  "Path to user's w3m data store.")

(defconst *user-w3m-cache-directory*
  (path-join *user-cache-directory* "w3m")
  "Path to user's w3m cache store.")


(defun user/w3m-display-hook (url)
  "w3m display hook for URL."
  (rename-buffer
   (format "*w3m: %s*" (or w3m-current-title w3m-current-url) 50) t))


(defun user/w3m-init ()
  "Initialize w3m."
  (setq-default
   ;; Set up data paths.
   w3m-bookmark-file (path-join *user-w3m-data-directory* "bookmarks.html")
   ;; Set up cache paths.
   w3m-cookie-file (path-join *user-w3m-cache-directory* "cookies")
   w3m-session-file (path-join *user-w3m-cache-directory* "sessions")
   ;; Use cookies.
   w3m-use-cookies t
   w3m-cookie-accept-bad-cookies t
   ;; Default to UTF-8 encoding.
   w3m-coding-system 'utf-8
   w3m-file-coding-system 'utf-8
   w3m-file-name-coding-system 'utf-8
   w3m-input-coding-system 'utf-8
   w3m-output-coding-system 'utf-8
   w3m-terminal-coding-system 'utf-8
   ;; Automatically restore crashed sessions.
   w3m-session-load-crashed-sessions t
   ;; Display page title in header line.
   w3m-use-header-line-title t
   ;; Set the default URL browser to w3m.
   browse-url-browser-function 'w3m-browse-url
   ;; Automatically cache all URLs.
   url-automatic-caching t)

  (when (display-graphic-p)
    (setq-default
     ;; Display graphics in pages.
     w3m-toggle-inline-images-permanently t
     w3m-default-display-inline-images t
     ;; Show favicons.
     w3m-use-favicon t
     w3m-favicon-use-cache-file t
     ;; Show graphical status indicator in mode line.
     w3m-show-graphic-icons-in-mode-line t))

  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
  (define-key user/utilities-map (kbd "b") 'w3m)
  (define-key user/utilities-map (kbd "B") 'browse-url-at-point)

  (add-hook 'w3m-display-hook 'user/w3m-display-hook))

(when *has-w3m*
  (require-package '(:name emacs-w3m :after (user/w3m-init))))


(provide 'apps/w3m)
;;; w3m.el ends here
