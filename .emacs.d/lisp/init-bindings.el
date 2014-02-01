;;; init-bindings.el --- sets up basic Emacs bindings
;;; Commentary:
;;; Code:

;; Define command groups
(defvar user/navigation-map nil
  "Map for navigational bindings.")

(defvar user/view-map nil
  "Map for view bindings.")

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
(defcustom user/navigation-prefix (kbd "C-c n")
  "Keyboard prefix to use for navigation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/view-prefix (kbd "C-x v")
  "Keyboard prefix to use for view commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/help-prefix (kbd "C-c h")
  "Keyboard prefix to use for help commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/documentation-prefix (kbd "C-c d")
  "Keyboard prefix to use for documentation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/code-prefix (kbd "C-c c")
  "Keyboard prefix to use for code manipulation commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/vcs-prefix (kbd "C-c v")
  "Keyboard prefix to use for version control commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/utilities-prefix (kbd "C-c u")
  "Keyboard prefix to use for utility commands."
  :type 'key-sequence
  :group 'user)

(defcustom user/apps-prefix (kbd "C-c a")
  "Keyboard prefix to use for application commands."
  :type 'key-sequence
  :group 'user)


(defvar user/global-keymap nil "Global keymap.")

(defun user/get-key (group name)
  "Get the key from GROUP to bind for NAME."
  (let ((key (cdr (assq name (cdr (assq group user/global-keymap))))))
    (kbd (if (listp key)
             (mapconcat 'identity (mapcar 'eval key) " ")
           key))))

(defun user/bind-key-global (group key function)
  "Bind GROUP KEY to FUNCTION globally."
  (global-set-key (user/get-key group key) function))

(defun user/bind-key-local (group key function)
  "Bind GROUP KEY to FUNCTION in the current keymap."
  (local-set-key (user/get-key group key) function))

(setq user/global-keymap
      `(;;; (Basic keys) ;;;
        (:basic . ((:open-file . "C-x C-f")
                   (:open-file-context . "C-x f")
                   (:open-buffer . "C-x b")
                   (:list-buffers . "C-x C-b")

                   (:save . "C-x C-s")
                   (:save-as . "C-x M-s")
                   (:close . "C-x k")
                   (:quit . "C-x C-c")

                   (:undo . "C-_")
                   (:redo . "M-_")

                   (:search-forward . "C-s")
                   (:search-backward . "C-r")
                   (:search-files . (user/navigation-prefix "f"))

                   (:selection-start . "C-SPC")
                   (:selection-expand . "C-=")
                   (:copy . "C-x C-w")
                   (:cut . "C-x C-k")
                   (:paste . "C-y")
                   (:cycle-paste . "M-y")
                   (:cut-word-left . "C-w")
                   (:cut-word-right . "M-w")

                   (:zoom . (user/view-prefix "z"))))

        ;;; (Emacs) ;;;
        (:emacs . ((:describe-bindings . (user/help-prefix "b"))
                   (:describe-coding . (user/help-prefix "C"))
                   (:describe-char . (user/help-prefix "c"))
                   (:describe-function . (user/help-prefix "f"))
                   (:describe-key . (user/help-prefix "k"))
                   (:describe-key-extensive . (user/help-prefix "K"))
                   (:describe-variable . (user/help-prefix "v"))
                   (:describe-language . (user/help-prefix "L"))
                   (:describe-mode . (user/help-prefix "m"))
                   (:describe-symbol . (user/help-prefix "s"))
                   (:describe-syntax . (user/help-prefix "S"))

                   (:find-package . (user/help-prefix "p"))
                   (:manual . (user/help-prefix "M"))
                   (:tutorial . (user/help-prefix "t"))
                   (:where-is . (user/help-prefix "w"))

                   (:grow-vertical . "C-c <up>")
                   (:shrink-vertical . "C-c <down>")
                   (:grow-horizontal . "C-c <right>")
                   (:shrink-horizontal . "C-c <left>")))

        ;;; (Documentation) ;;;
        (:docs . ((:apropos . (user/documentation-prefix "SPC"))

                  (:describe . (user/documentation-prefix "d"))
                  (:describe-function . (user/documentation-prefix "f"))
                  (:describe-variable . (user/documentation-prefix "v"))

                  (:reference . (user/documentation-prefix "r"))))

        ;;; (Navigation) ;;;
        (:navigation . ((:context . (user/navigation-prefix "SPC"))

                        (:goto-line . (user/navigation-prefix "g"))
                        (:go-back . (user/navigation-prefix "b"))

                        (:context-forward . (user/navigation-prefix "s f"))
                        (:context-backward . (user/navigation-prefix "s b"))
                        (:context-up . (user/navigation-prefix "s p"))
                        (:context-down . (user/navigation-prefix "s n"))

                        (:next . (user/navigation-prefix "n"))

                        (:follow-symbol . (user/navigation-prefix "j"))
                        (:find-symbol . (user/navigation-prefix "s"))
                        (:jump-spec-impl . (user/navigation-prefix "i"))
                        (:references . (user/navigation-prefix "r"))
                        (:switch-spec-impl . (user/navigation-prefix "h"))
                        (:functions/toc . (user/navigation-prefix "t"))

                        (:open . (user/utilities-prefix "o"))))

        ;;; (Programming) ;;;
        (:code . ((:compile . (user/code-prefix "c"))
                  (:run . (user/code-prefix "r"))
                  (:test . (user/code-prefix "t"))

                  (:comment . (user/code-prefix "-"))
                  (:fill-paragraph . (user/code-prefix "f"))

                  (:complete . (user/code-prefix "SPC"))
                  (:try-complete . "TAB")

                  (:context-promote . (user/code-prefix "P"))
                  (:context-demote . (user/code-prefix "N"))

                  (:warnings/errors . (user/code-prefix "E"))
                  (:spellcheck-word . (user/code-prefix "s"))
                  (:spellcheck-add-word . (user/code-prefix "S"))

                  (:update-index . (user/code-prefix "i"))

                  (:eval-expression . ("C-x C-e"))
                  (:eval-buffer . (user/code-prefix "e b"))
                  (:eval-function . (user/code-prefix "e f"))
                  (:eval-selection . (user/code-prefix "e s"))

                  (:virtual . (user/code-prefix "v"))))

        ;;; (Debugging) ;;;
        (:debug . ((:start . (user/code-prefix "d"))

                   (:break . (user/code-prefix "b"))
                   (:break-temporary . (user/code-prefix "t"))
                   (:watch . (user/code-prefix "w"))

                   (:run . (user/code-prefix "r"))
                   (:continue . (user/code-prefix "c"))
                   (:continue-stack . (user/code-prefix "f"))
                   (:continue-until . (user/code-prefix "u"))
                   (:step . (user/code-prefix "s"))
                   (:step-instruction . (user/code-prefix "i"))
                   (:next . (user/code-prefix "n"))

                   (:stack-up . (user/code-prefix "p"))
                   (:stack-down . (user/code-prefix "n"))
                   (:show-value . (user/code-prefix "p"))))

        ;;; (Version Control) ;;;
        (:vcs . ((:status . (user/vcs-prefix "s"))
                 (:describe . (user/vcs-prefix "d"))
                 (:gutter . (user/vcs-prefix "g"))))

        ;;; (Utilities) ;;;
        (:util . ((:ace-jump-mode . (user/navigation-prefix "a"))

                  (:ecb-toggle . (user/utilities-prefix "e"))

                  (:google . (user/utilities-prefix "g"))
                  (:google-at-point . (user/documentation-prefix "g RET"))
                  (:google-selection . (user/documentation-prefix "g SPC"))

                  (:macrostep-expand . (user/code-prefix "e m"))

                  (:minimap . (user/view-prefix "m"))

                  (:popwin-close . (user/view-prefix "0"))
                  (:popwin-buffer . (user/view-prefix "p"))
                  (:popwin-messages . (user/view-prefix "m"))

                  (:undo-tree . (user/utilities-prefix "u"))

                  (:wc-mode . (user/utilities-prefix "w"))))

        ;;; (Applications) ;;;
        (:apps . ((:browser . (user/apps-prefix "b"))
                  (:calendar . (user/apps-prefix "c"))
                  (:email . (user/apps-prefix "e"))
                  (:instant-messenger . (user/apps-prefix "i"))
                  (:notes . (user/apps-prefix "n"))

                  (:ipython-notebook . (user/apps-prefix "p"))

                  (:music . (user/apps-prefix "m"))

                  (:statistics . (user/apps-prefix "s"))))))


(defun user/bindings-init ()
  "Initialize key bindings."
  ;; Bind keys to maps
  (define-prefix-command 'user/navigation-map)
  (define-key global-map user/navigation-prefix 'user/navigation-map)

  (define-prefix-command 'user/view-map)
  (define-key global-map user/view-prefix 'user/view-map)

  (define-prefix-command 'user/help-map)
  (define-key global-map user/help-prefix 'user/help-map)

  (define-prefix-command 'user/documentation-map)
  (define-key global-map user/documentation-prefix 'user/documentation-map)

  (define-prefix-command 'user/code-map)
  (define-key global-map user/code-prefix 'user/code-map)

  (define-prefix-command 'user/vcs-map)
  (define-key global-map user/vcs-prefix 'user/vcs-map)

  (define-prefix-command 'user/utilities-map)
  (define-key global-map user/utilities-prefix 'user/utilities-map)

  ;;; (Bindings) ;;;
  ;; Alias C-x C-m to M-x which is a bit awkward to reach
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  (global-set-key (kbd "C-x m") 'execute-extended-command))

(user/bindings-init)


(provide 'init-bindings)
;;; init-bindings.el ends here
