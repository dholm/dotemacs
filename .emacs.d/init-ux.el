;;; init-ux.el --- initializes user experience
;;; Commentary:
;;; Code:

(load-all-files-from-dir (path-join user-emacs-directory "ux"))

;; Font when in graphics mode
(when (display-graphic-p)
  (if (eq system-type 'windows-nt)
      (set-default-font "consolas-10")
    (set-default-font "Meslo LG S DZ:pixelsize=10:foundry=bitstream:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")))


;; Override Darwin
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")


;; Remove all the mouse-assisted crud
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


(setq-default
 ;; Do not show the splash screen or message
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 ;; Inhibit GUI features
 use-file-dialog nil
 user-dialog-box nil
 ;; Show row and column numbers
 line-number-mode t
 column-number-mode t)


;; Show matching parenthesis
(show-paren-mode t)


;; Set random parameters
(setq search-highlight t        ; Highlight all visible matches
      query-replace-highlight t ; Highlight all visible matches
      transient-mark-mode t)    ; Perform certain commands only on the marked region


;; Properly display colors in shell
(ansi-color-for-comint-mode-on)


;; Display the current time and system load
(require 'time)
(setq display-time-24hr-format t
      display-time-form-list (list 'time 'load)
      display-time-day-and-date t)
(display-time)


;; Enable blinking cursor
(blink-cursor-mode)


(provide 'init-ux)
;;; init-ux.el ends here
