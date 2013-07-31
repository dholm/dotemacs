;;; shell.el --- initializes shell modes
;;; Commentary:
;;; Code:

(defun dholm/sh-mode-hook ()
  "Initialize mode for shell script editing."
  (setq-default
   ;; Indent with four spaces
   sh-basic-offset 4
   sh-indentation 4))

(defun dholm/shell-mode-hook ()
  "Initialize mode for interactive shell."
  (setq-default
   ;; Set up to use Bash with input echoing
   explicit-shell-file-name "bash"
   explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
   comint-process-echoes t)
  ;; Enable ANSI colors for Comint
  (ansi-color-for-comint-mode-on)
  ;; Enable readline completion
  (require 'readline-complete)
  (ac-rlc-setup-sources))

(add-hook 'sh-mode-hook 'dholm/sh-mode-hook)
(add-hook 'shell-mode-hook 'dholm/shell-mode-hook)


(defun dholm/readline-complete-init ()
  "Initialize readline complete."
  (add-to-list 'ac-modes 'shell-mode))

(require-package '(:name bash-completion))
(require-package '(:name readline-complete :after (dholm/readline-complete-init)))
(require-package '(:name shell-command
			 :type emacswiki
			 :website "https://raw.github.com/emacsmirror/emacswiki.org/master/shell-command.el"))


(provide 'modes/shell)
;;; shell.el ends here
