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
  (scion-mode t))


(defun dholm/inferior-haskell-mode-hook ()
  "Inferior Haskell mode hook."
  (dholm/generic-haskell-mode-hook)
  (turn-on-ghci-completion))


(defun dholm/haskell-mode-init ()
  "Initialize haskell mode."
  (add-hook 'haskell-mode-hook 'dholm/haskell-mode-hook)
  (add-hook 'inferior-haskell-mode-hook 'dholm/inferior-haskell-mode-hook))


(require-package '(:name haskell-mode
			 :type github
			 :pkgname "haskell/haskell-mode"
			 :load "haskell-mode-autoloads.el"
			 :build (("make" "all"))
                         :after (dholm/haskell-mode-init)))
(require-package '(:name ghci-completion
			 :type github
			 :pkgname "manzyuk/ghci-completion"))
(require-package '(:name scion
                         :type github
                         :pkgname "nominolo/scion"
                         :load-path "emacs"
                         :prepare (progn
                                    (autoload 'scion-mode "scion"))))


(provide 'modes/haskell)
;;; haskell.el ends here
