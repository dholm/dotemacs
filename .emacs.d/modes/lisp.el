;; (Code Conventions) ;;

(defun dholm/lisp-mode-hook ()
  ;; Enable CEDET
  (dholm/cedet-hook)
  (rainbow-delimiters-mode)
  (enable-paredit-mode)
  (turn-on-eldoc-mode)
  (redshank-mode)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))

(defun dholm/emacs-lisp-mode-hook ()
  (dholm/lisp-mode-hook)
  (elisp-slime-nav-mode)
  (ac-emacs-lisp-mode-setup)
  (local-set-key (kbd "C-c e") 'macrostep-expand))

(add-hook 'lisp-mode-hook 'dholm/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'dholm/emacs-lisp-mode-hook)


(provide 'modes/lisp)
