;;; haskell --- initializes Haskell modes
;;; Commentary:
;;; Code:

(defun dholm/generic-haskell-mode-hook ()
  "Generic Haskell mode hook."
  (turn-on-haskell-doc-mode)
  ;; Enable editing of camel case
  (subword-mode +1)
  ;; Bindings
  (define-key dholm/documentation-map (kbd "h") 'hoogle))


(defun dholm/haskell-mode-hook ()
  "Haskell mode hook."
  (dholm/generic-haskell-mode-hook)
  (turn-on-haskell-indentation)
  ;; Enable scion
  (scion-mode t)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))


(defun dholm/inferior-haskell-mode-hook ()
  "Inferior Haskell mode hook."
  (dholm/generic-haskell-mode-hook)
  (turn-on-ghci-completion))


(add-hook 'haskell-mode-hook 'dholm/haskell-mode-hook)
(add-hook 'inferior-haskell-mode-hook 'dholm/inferior-haskell-mode-hook)


(require-package '(:name haskell-mode
			 :type github
			 :pkgname "haskell/haskell-mode"
			 :load "haskell-mode-autoloads.el"
			 :build (("make" "all"))
			 :post-init (progn
				      (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
				      (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
(require-package '(:name ghci-completion
			 :type github
			 :pkgname "manzyuk/ghci-completion"))
(require-package '(:name scion))


(provide 'modes/haskell)
;;; haskell.el ends here
