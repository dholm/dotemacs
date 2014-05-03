;;; shell.el --- initializes shell modes
;;; Commentary:
;;; Code:

(defconst *shell-cache-directory* (path-join *user-cache-directory*
                                             "shell"))


(defun user/sh-mode-hook ()
  "Initialize mode for shell script editing."
  (setq-default
   ;; Indent with four spaces.
   sh-basic-offset 4
   sh-indentation 4)

  ;; Register file types with find-file-in-project.
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.sh")))


(defun user/shell-mode-common-hook ()
  "Shell mode common hook."
  (with-feature 'ansi-color
    ;; Enable ANSI colors for comint.
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

  ;; Enable readline completion.
  (with-feature 'readline-complete
    (ac-rlc-setup-sources)))


(defun user/shell-mode-hook ()
  "Initialize mode for interactive shell."
  (user/shell-mode-common-hook)

  (setq-default
   ;; Set up to use Bash with input echoing.
   explicit-shell-file-name "bash"
   explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
   comint-process-echoes t))


(defun user/eshell-mode-hook ()
  "Mode hook for eshell."
  (user/shell-mode-common-hook)

  (turn-on-eldoc-mode)

  ;; Use auto-complete for completion.
  (add-ac-sources 'ac-source-pcomplete)
  (add-ac-modes 'eshell-mode))


(defun user/shell-prompt ()
  "Return a prompt for the shell."
  (concat
   (with-face (concat (eshell/pwd) " ")
              :inherit 'header-line)
   (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time))
              :inherit 'header-line)
   (with-face
    (or (ignore-errors
          (format "(%s)" (vc-responsible-backend default-directory))) "")
    :inherit 'header-line)
   (with-face "\n" :inherit 'header-line)
   (with-face user-login-name :foreground "blue")
   "@"
   (with-face "localhost" :foreground "green")
   (if (= (user-uid) 0)
       (with-face " #" :foreground "red")
     " $")
   " "))


(defun user/eshell-init ()
  "Initialize the Emacs shell."
  (setq-default
   ;; Set the path to the shell cache store.
   eshell-directory-name *shell-cache-directory*
   ;; Set eshell prompt.
   eshell-prompt-function 'user/shell-prompt
   eshell-prompt-regexp "^[^#$\n]*[#$] "
   ;; Set a decent history size.
   eshell-history-size 10000
   ;; Announce the terminal type.
   eshell-term-name "eterm-color"
   ;; Allow using buffer names directly in redirection.
   eshell-buffer-shorthand t)

  (after-load 'esh-module
    (add-many-to-list 'eshell-modules-list
                      ;; Rebind keys while point is in a region of input text.
                      'eshell-rebind
                      ;; Smart command output management.
                      'eshell-smart
                      ;; Extra alias functions.
                      'eshell-xtra))

  (after-load 'em-term
    ;; Commands that should be run using term for better handling of ANSI control
    ;; codes.
    (add-many-to-list 'eshell-visual-commands
                      "htop" "perf" "ssh" "telnet" "tmux"))

  (add-hook 'eshell-mode-hook 'user/eshell-mode-hook)

  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :shell 'eshell))


(defun user/readline-complete-init ()
  "Initialize readline complete."
  (add-ac-modes 'shell-mode 'eshell-mode))


(defun user/shell-mode-init ()
  "Initialize shell modes."
  (user/eshell-init)

  ;;; (Packages) ;;;
  (require-package '(:name bash-completion))
  (require-package '(:name readline-complete :after (user/readline-complete-init)))
  (require-package '(:name shell-command
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/shell-command.el"))
  (require-package '(:name eshell-manual))

  (add-hook 'sh-mode-hook 'user/sh-mode-hook)
  (add-hook 'shell-mode-hook 'user/shell-mode-hook))

(user/shell-mode-init)


(provide 'modes/shell)
;;; shell.el ends here
