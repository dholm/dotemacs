;;; init-bindings.el --- sets up basic Emacs bindings
;;; Commentary:
;;; Code:

;; Set up prefixes for groups of commands
(define-prefix-command 'user/navigation-map)
(define-prefix-command 'user/code-map)
(define-prefix-command 'user/documentation-map)
(define-prefix-command 'user/vcs-map)
(define-prefix-command 'user/utilities-map)

(global-set-key (kbd "C-c n") 'user/navigation-map)
(global-set-key (kbd "C-c c") 'user/code-map)
(global-set-key (kbd "C-c d") 'user/documentation-map)
(global-set-key (kbd "C-c v") 'user/vcs-map)
(global-set-key (kbd "C-c u") 'user/utilities-map)


;; Bookmarks
(global-set-key (kbd "C-c b") 'bookmark-map)


;; Binds goto-line to navigation command g which is easier to access
(define-key user/navigation-map (kbd "g") 'goto-line)


;; Alias C-x C-m to M-x which is a bit awkward to reach
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)


;; Delete words with C-w and rebind kill region to C-x C-k
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)


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
