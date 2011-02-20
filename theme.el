;; (Theme) ;;

;; Load the support package for color themes
(setq load-path (cons "~/.emacs.d/vendor/color-theme" load-path))
(require 'color-theme)

;; Load and set the color theme "Subdued"
(setq load-path (cons "~/.emacs.d/vendor/color-theme-subdued" load-path))
(require 'color-theme-subdued)
(color-theme-subdued)

;; Override Darwin
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")


;; Do not show the splash screen
(setq inhibit-splash-screen t)


;; Remove all the mouse-assisted crud
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; Highlighting "TODO", "FIXME" and friends
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|TODO\\|BUG\\):"
				       1 font-lock-warning-face t)))))


;; Show matching parenthesis
(show-paren-mode t)


;; Prefer using UTF-8 encoding
(prefer-coding-system 'utf-8)


;; Set random parameters
(setq search-highlight t        ; Highlight all visible matches
      query-replace-highlight t ; Highlight all visible matches
      transient-mark-mode t)    ; Perform certain commands only on the marked region
