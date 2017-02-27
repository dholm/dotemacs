;;; w3m.el --- W3M web browser support. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst *user-w3m-data-directory*
  (path-join *user-data-directory* "w3m")
  "Path to user's w3m data store.")

(defconst *user-w3m-cache-directory*
  (path-join *user-cache-directory* "w3m")
  "Path to user's w3m cache store.")


(defun user--w3m-display-hook (url)
  "W3M display hook for URL."
  (rename-buffer
   (format "*w3m: %s*" (or w3m-current-title w3m-current-url)) t)
  (let ((buffer-read-only nil))
    ;; Remove trailing whitespace when browsing.
    (delete-trailing-whitespace)))


(defun user/w3m-browse-current-buffer ()
  "Open the current buffer in w3m."
  (interactive)
  (if (file-readable-p (path-abs-buffer))
      (w3m-find-file (path-abs-buffer))
    (let ((filename (concat (make-temp-file "w3m-") ".html")))
      (try-eval
          (progn
            (write-region (point-min) (point-max) filename)
            (w3m-find-file filename))
        (delete-file filename)))))

(unless (feature-p 'eww)
  (with-executable 'w3m
    (use-package w3m
      :defer
      :init
      (autoload 'w3m-browse-url "w3m" nil t)
      (add-hook 'w3m-display-hook 'user--w3m-display-hook)
      :config
      (validate-setq
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
       ;; Use UTF-8 to display nicer tables.
       w3m-default-symbol
       '("─┼" " ├" "─┬" " ┌" "─┤" " │" "─┐" ""
         "─┴" " └" "──" ""   "─┘" ""   ""   ""
         "─┼" " ┠" "━┯" " ┏" "─┨" " ┃" "━┓" ""
         "━┷" " ┗" "━━" ""   "━┛" ""   ""   ""
         " •" " □" " ☆" " ○" " ■" " ★" " ◎"
         " ●" " △" " ●" " ○" " □" " ●" "≪ ↑ ↓ "))

      (when (display-graphic-p)
        (validate-setq
         ;; Display graphics in pages.
         w3m-toggle-inline-images-permanently t
         w3m-default-display-inline-images t
         ;; Show favicons.
         w3m-use-favicon t
         w3m-favicon-use-cache-file t
         ;; Show graphical status indicator in mode line.
         w3m-show-graphic-icons-in-mode-line t)))))


(provide 'apps/w3m)
;;; w3m.el ends here
