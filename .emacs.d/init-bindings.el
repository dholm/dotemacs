;;; init-bindings --- sets up basic Emacs bindings
;;; Commentary:
;;; Code:

;; Set up prefixes for groups of commands
(define-prefix-command 'dholm/navigation-map)
(define-prefix-command 'dholm/documentation-map)
(define-prefix-command 'dholm/vcs-map)

(global-set-key (kbd "C-c n") 'dholm/navigation-map)
(global-set-key (kbd "C-c d") 'dholm/documentation-map)
(global-set-key (kbd "C-c v") 'dholm/vcs-map)


;; Function keys
(global-set-key [f7] 'compile)
(global-set-key [f8] 'gdb)


;; Binds goto-line to navigation command g which is easier to access
(define-key dholm/navigation-map (kbd "g") 'goto-line)


;; Alias C-x C-m to M-x which is a bit awkward to reach
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)


;; Delete words with C-w and rebind kill region to C-x C-k
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)


;; Spell check the current word
(global-set-key (kbd "C-c C-w") 'ispell-word)


;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


;; Resizing windows
(global-set-key (kbd "C-c <up>") 'shrink-window)
(global-set-key (kbd "C-c <down>") 'enlarge-window)
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)


(provide 'init-bindings)
;;; init-bindings.el ends here
