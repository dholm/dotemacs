;; (Theme) ;;


;; Load and set the color theme "Zenburn"
(if (>= emacs-major-version 24)
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/zenburn-emacs")
    (load-theme 'zenburn t))
  (progn
    (setq load-path (cons "~/.emacs.d/vendor/zenburn-emacs" load-path))
    (require 'color-theme-zenburn)
    (when (featurep 'color-theme-zenburn)
    (color-theme-zenburn))))


;; Override Darwin
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")


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


;; Properly display colors in shell
(ansi-color-for-comint-mode-on)
