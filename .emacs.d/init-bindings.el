;;; init-bindings.el --- sets up basic Emacs bindings
;;; Commentary:
;;; Code:

;; Set up prefixes for groups of commands
(defcustom user/navigation-keyboard-prefix (kbd "C-c n")
  "Keyboard prefix to use for navigation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/documentation-keyboard-prefix (kbd "C-c d")
  "Keyboard prefix to use for documentation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/code-keyboard-prefix (kbd "C-c c")
  "Keyboard prefix to use for code manipulation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/vcs-keyboard-prefix (kbd "C-c v")
  "Keyboard prefix to use for version control commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/utilities-keyboard-prefix (kbd "C-c u")
  "Keyboard prefix to use for utility commands."
  :type 'key-sequence
  :group 'user)

(define-prefix-command 'user/navigation-map)
(define-key global-map user/navigation-keyboard-prefix 'user/navigation-map)

(define-prefix-command 'user/documentation-map)
(define-key global-map user/documentation-keyboard-prefix 'user/documentation-map)

(define-prefix-command 'user/code-map)
(define-key global-map user/code-keyboard-prefix 'user/code-map)

(define-prefix-command 'user/vcs-map)
(define-key global-map user/vcs-keyboard-prefix 'user/vcs-map)

(define-prefix-command 'user/utilities-map)
(define-key global-map user/utilities-keyboard-prefix 'user/utilities-map)


;; Bind bookmarks to C-c b
(global-set-key (kbd "C-c b") 'bookmark-map)


;; Binds goto-line to navigation command g which is easier to access than M-g g
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
