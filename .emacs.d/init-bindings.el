;;; init-bindings.el --- sets up basic Emacs bindings
;;; Commentary:
;;; Code:

;; Define command groups
(defvar user/navigation-map nil
  "Map for navigational bindings.")

(defvar user/documentation-map nil
  "Map for bindings to access documentation.")

(defvar user/code-map nil
  "Map for bindings that interact with or modify code.")

(defvar user/vcs-map nil
  "Map for bindings to interact with version control systems.")

(defvar user/utilities-map nil
  "Map for bindings to interact with utility modules.")


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


;; Bind keys to maps
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


;; Alias C-x C-m to M-x which is a bit awkward to reach
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)


(provide 'init-bindings)
;;; init-bindings.el ends here
