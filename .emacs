;; (Coloring and Themes) ;;

(setq load-path (cons "~/.emacs.d" load-path))

(require 'color-theme-subdued)
(color-theme-subdued)

;; Override Darwin
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")


;; Highlighting "TODO", "FIXME" and friends
(add-hook 'c-mode-common-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 
        1 font-lock-warning-face t)))))


;; (Utilities) ;;

;; Bring up the buffer menu with §
(global-set-key [?§] 'buffer-menu)


;; Show line numbers with F6
;; http://stud4.tuwien.ac.at/~e0225855/linum/linum.html
(require 'linum)
(global-set-key (kbd "<f6>") 'linum-mode)


;; Show trailing whitespace
(setq show-trailing-whitespace t)


;; Magit
(setq load-path (cons "~/.emacs.d/magit" load-path))
(require 'magit)


;; Hippie Expand
;; The order that different completes are tested.
(global-set-key (kbd "C-<tab>") 'hippie-expand)

(setq hippie-expand-try-functions-list
  '(try-expand-dabbrev-visible
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    ; try-expand-all-abbrevs
    ; try-expand-line 
))



;; (Code Conventions) ;;

(setq c-default-style "Linux")
(setq c++-default-style "Stroustrup")

(setq load-path (cons "~/.emacs.d/dtrt-mode" load-path))
(add-hook 'c-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

(add-hook 'c++-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

