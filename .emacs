;; (Coloring and Themes) ;;

(setq load-path (cons "~/.emacs.d" load-path))

(require 'color-theme-subdued)
(color-theme-subdued)

;; Override Darwin
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")


;; Do not show the splash screen
(setq inhibit-splash-screen t)


;; Highlighting "TODO", "FIXME" and friends
(add-hook 'c-mode-common-hook
  (lambda ()
    (font-lock-add-keywords nil
      '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 
        1 font-lock-warning-face t)))))


;; (Utilities) ;;

;; Bring up the buffer menu with §
(global-set-key [?§] 'buffer-menu)


;; cycle through buffers with Ctrl-Tab
(require 'stesla-rotate-buffers)
(global-set-key (kbd "C-<tab>") 'stesla-rotate-buffers)
(global-set-key (kbd "C-M-<tab>") (lambda ()
  (interactive)
    (stesla-rotate-buffers -1)))

;; Show line numbers with F6
;; http://stud4.tuwien.ac.at/~e0225855/linum/linum.html
(require 'linum)
(global-set-key (kbd "<f6>") 'linum-mode)


;; Show trailing whitespace
(setq show-trailing-whitespace t)


;; Magit
(setq load-path (cons "~/.emacs.d/magit" load-path))
(require 'magit)


;; Enable both HippieCompletion and indent for tab
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
    (dabbrev-expand nil)
    (indent-for-tab-command)))

;; Tab completion
(let (modelist it)
  (setq modelist '(
    'c-mode-common-hook
    'lisp-mode-common-hook
    'text-mode-hook
    'java-mode-hook
    'emacs-lisp-mode-hook
    'LaTex-mode-hook
    'Tex-Mode-hook
    'python-mode-hook
    'octave-mode-hook))
    (setq it modelist)
    (while it
      (add-hook (eval (car it)) (function (lambda ()
        (local-set-key (kbd "<tab>") 'indent-or-complete))))
      (add-hook (eval (car it)) (function (lambda ()
        (local-set-key (kbd "TAB") 'indent-or-complete))))
      (setq it (cdr it))))

;; The order that different completes are tested.
(setq hippie-expand-try-functions-list
  '(try-expand-dabbrev-visible
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-expand-all-abbrevs
    try-expand-line))


;; (Code Conventions) ;;

(setq c-default-style "K&R")
(setq c++-default-style "Stroustrup")
(setq show-trailing-whitespace t)

(setq load-path (cons "~/.emacs.d/dtrt-indent" load-path))
(add-hook 'c-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

(add-hook 'c++-mode-common-hook
  (lambda()
    (require 'dtrt-indent)
    (dtrt-indent-mode t)))

