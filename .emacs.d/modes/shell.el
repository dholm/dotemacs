;;; shell.el --- initializes shell modes
;;; Commentary:
;;; Code:

(defun user/sh-mode-hook ()
  "Initialize mode for shell script editing."
  (setq
   ;; Indent with four spaces
   sh-basic-offset 4
   sh-indentation 4)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.sh")))


(defun user/shell-mode-hook ()
  "Initialize mode for interactive shell."
  (setq
   ;; Set up to use Bash with input echoing
   explicit-shell-file-name "bash"
   explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
   comint-process-echoes t)
  ;; Enable readline completion
  (require 'readline-complete)
  (ac-rlc-setup-sources))


(defun user/readline-complete-init ()
  "Initialize readline complete."
  (add-to-list 'ac-modes 'shell-mode))


(defun user/shell-mode-init ()
  "Initialize shell modes."
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
