;; (Code Conventions) ;;

(defun dholm/lisp-mode-hook ()
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))
(add-hook 'lisp-mode-hook 'dholm/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'dholm/lisp-mode-hook)
