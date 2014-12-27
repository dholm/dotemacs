;;; shell.el --- initializes shell modes
;;; Commentary:
;;; Code:

(defun user/sh-mode-hook ()
  "Initialize mode for shell script editing."
  (setq-default
   ;; Indent with four spaces.
   sh-basic-offset 4
   sh-indentation 4))


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


(defun user/readline-complete-init ()
  "Initialize readline complete."
  (add-ac-modes 'shell-mode 'eshell-mode))


(defun user/shell-mode-init ()
  "Initialize shell modes."

  ;;; (Packages) ;;;
  (require-package '(:name bash-completion))
  (require-package '(:name readline-complete :after (user/readline-complete-init)))
  (require-package '(:name shell-command
                           :type emacswiki
                           :website "https://raw.github.com/emacsmirror/emacswiki.org/master/shell-command.el"))

  (add-hook 'sh-mode-hook 'user/sh-mode-hook)
  (add-hook 'shell-mode-hook 'user/shell-mode-hook))

(user/shell-mode-init)


(provide 'modes/shell)
;;; shell.el ends here
