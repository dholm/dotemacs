;;; (Initialization) ;;;
(require-package '(:name bash-completion
			 :type github
			 :pkgname "szermatt/emacs-bash-completion"
			 :post-init (progn
				      (add-hook 'shell-dynamic-complete-functions
						'bash-completion-dynamic-complete)
				      (add-hook 'shell-command-complete-functions
						'bash-completion-dynamic-complete))))
(require-package '(:name shell-command
			 :type emacswiki
			 :website "https://raw.github.com/emacsmirror/emacswiki.org/master/shell-command.el"))


;;; (Code Conventions) ;;;
(defun dholm/shell-mode-hook ()
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))

(add-hook 'shell-mode-hook 'dholm/shell-mode-hook)


(provide 'modes/shell)
