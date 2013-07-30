;;; shell --- initializes shell modes
;;; Commentary:
;;; Code:

(defun dholm/sh-mode-hook ()
  "Initialize sh mode."
  (setq-default
   ;; Indent with four spaces
   sh-basic-offset 4
   sh-indentation 4)
  ;; Enable ANSI colors for comint
  (ansi-color-for-comint-mode-on))

(add-hook 'sh-mode-hook 'dholm/sh-mode-hook)


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


(provide 'modes/shell)
;;; shell.el ends here
