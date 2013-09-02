;;; haskell.el --- initializes Haskell modes
;;; Commentary:
;;; Code:

(defconst *has-ghc* (executable-find "ghc"))


(defun user/generic-haskell-mode-hook ()
  "Generic Haskell mode hook."
  (turn-on-haskell-doc-mode)
  ;; Enable editing of camel case
  (subword-mode +1)
  ;; Bindings
  (define-key user/documentation-map (kbd "h") 'hoogle))


(defun user/haskell-mode-hook ()
  "Haskell mode hook."
  (user/generic-haskell-mode-hook)
  (turn-on-haskell-indentation)

  ;; Register file types with find-file-in-project
  (after-load 'find-file-in-project
    (user/ffip-local-patterns "*.hs" "*.lhs"))

  ;; Enable scion
  (scion-mode t))


(defun user/inferior-haskell-mode-hook ()
  "Inferior Haskell mode hook."
  (user/generic-haskell-mode-hook)
  (turn-on-ghci-completion))


(defun user/haskell-mode-init ()
  "Initialize haskell mode."
  (require-package '(:name ghci-completion))
  (require-package '(:name scion
                           :type github
                           :pkgname "nominolo/scion"
                           :load-path "emacs"
                           :prepare (progn
                                      (autoload 'scion-mode "scion"))))

  (add-hook 'haskell-mode-hook 'user/haskell-mode-hook)
  (add-hook 'inferior-haskell-mode-hook 'user/inferior-haskell-mode-hook))

(when *has-ghc*
  (require-package '(:name haskell-mode
                           :type github
                           :pkgname "haskell/haskell-mode"
                           :load "haskell-mode-autoloads.el"
                           :build (("make" "all"))
                           :after (user/haskell-mode-init))))


(provide 'modes/haskell)
;;; haskell.el ends here
