;; (Theme) ;;

;; Load and set the color theme "Subdued"
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
(show-paren-mode 1)
