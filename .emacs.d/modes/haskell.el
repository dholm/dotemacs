;; Haskell mode

(require-package (:name haskell-mode
                        :type github
                        :pkgname "haskell/haskell-mode"
                        :load "haskell-mode-autoloads.el"
                        :build (("make" "all"))
                        :post-init (progn
                                     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                                     (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))))
(require-package (:name ghci-completion
                        :type github
                        :pkgname "manzyuk/ghci-completion"))
(require-package (:name scion))


(defun dholm/generic-haskell-mode-hook ()
  (turn-on-haskell-doc-mode))


(defun dholm/haskell-mode-hook ()
  (dholm/generic-haskell-mode-hook)
  (turn-on-haskell-indentation)
  ;; Enable editing of camel case
  (subword-mode +1)
  ;; Bindings
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  ;; Run spell-checker on strings and comments
  (flyspell-prog-mode))


(defun dholm/inferior-haskell-mode-hook ()
  (dholm/generic-haskell-mode-hook)
  (turn-on-ghci-completion))


(add-hook 'haskell-mode-hook 'dholm/haskell-mode-hook)
(add-hook 'inferior-haskell-mode-hook 'dholm/inferior-haskell-mode-hook)


(provide 'modes/haskell)
