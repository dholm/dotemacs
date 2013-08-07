;;; haskell --- initializes Haskell modes
;;; Commentary:
;;; Code:

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
  ;; Enable scion
  (scion-mode t))


(defun user/inferior-haskell-mode-hook ()
  "Inferior Haskell mode hook."
  (user/generic-haskell-mode-hook)
  (turn-on-ghci-completion))


(defun user/haskell-mode-init ()
  "Initialize haskell mode."
  (add-hook 'haskell-mode-hook 'user/haskell-mode-hook)
  (add-hook 'inferior-haskell-mode-hook 'user/inferior-haskell-mode-hook))


(require-package '(:name haskell-mode
			 :type github
			 :pkgname "haskell/haskell-mode"
			 :load "haskell-mode-autoloads.el"
			 :build (("make" "all"))
                         :after (user/haskell-mode-init)))
(require-package '(:name ghci-completion))
(require-package '(:name scion
                         :type github
                         :pkgname "nominolo/scion"
                         :load-path "emacs"
                         :prepare (progn
                                    (autoload 'scion-mode "scion"))))


(provide 'modes/haskell)
;;; haskell.el ends here
