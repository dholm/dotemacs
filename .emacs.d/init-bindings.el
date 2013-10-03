;;; init-bindings.el --- sets up basic Emacs bindings
;;; Commentary:
;;; Code:

;; Define command groups
(defvar user/navigation-map nil
  "Map for navigational bindings.")

(defvar user/help-map nil
  "Map for bindings to access help.")

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

(defcustom user/help-keyboard-prefix (kbd "C-c h")
  "Keyboard prefix to use for help commands."
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


(defun user/bindings-init ()
  "Initialize key bindings."
  ;; Bind keys to maps
  (define-prefix-command 'user/navigation-map)
  (define-key global-map user/navigation-keyboard-prefix 'user/navigation-map)

  (define-prefix-command 'user/help-map)
  (define-key global-map user/help-keyboard-prefix 'user/help-map)

  (define-prefix-command 'user/documentation-map)
  (define-key global-map user/documentation-keyboard-prefix 'user/documentation-map)

  (define-prefix-command 'user/code-map)
  (define-key global-map user/code-keyboard-prefix 'user/code-map)

  (define-prefix-command 'user/vcs-map)
  (define-key global-map user/vcs-keyboard-prefix 'user/vcs-map)

  (define-prefix-command 'user/utilities-map)
  (define-key global-map user/utilities-keyboard-prefix 'user/utilities-map)

  ;;; (Bindings) ;;;
  ;; Alias C-x C-m to M-x which is a bit awkward to reach
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  (global-set-key (kbd "C-x m") 'execute-extended-command)

  ;; Help keys for bindings.
  (define-key user/help-map (kbd "b") 'describe-bindings)
  (define-key user/help-map (kbd "k") 'describe-key)
  (define-key user/help-map (kbd "K") 'Info-goto-emacs-key-command-node)
  (define-key user/help-map (kbd "w") 'where-is)

  ;; Help keys for Emacs.
  (define-key user/help-map (kbd "f") 'describe-function)
  (define-key user/help-map (kbd "v") 'describe-variable)
  (define-key user/help-map (kbd "p") 'finder-by-keyword)
  (define-key user/help-map (kbd "M") 'info-emacs-manual)
  (define-key user/help-map (kbd "s") 'info-lookup-symbol)

  ;; Help keys for mode.
  (define-key user/help-map (kbd "m") 'describe-mode)
  (define-key user/help-map (kbd "S") 'describe-syntax))

(user/bindings-init)


(provide 'init-bindings)
;;; init-bindings.el ends here
