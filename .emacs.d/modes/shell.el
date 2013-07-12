;;; (Initialization) ;;;
(require-package (:name bash-completion))
(require-package (:name shell-command))


;;; (Code Conventions) ;;;
(defun dholm/shell-mode-hook ()
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))

(add-hook 'shell-mode-hook 'dholm/shell-mode-hook)


(provide 'modes/shell)
