;;; haskell.el --- initializes Haskell modes
;;; Commentary:
;;; Code:

(defun user/generic-haskell-mode-hook ()
  "Generic Haskell mode hook."
  (turn-on-haskell-doc-mode)

  ;; Enable editing of camel case
  (subword-mode t)

  (with-feature 'ac-ghc-mod
    (add-ac-sources 'ac-source-ghc-module 'ac-source-ghc-symbol
                    'ac-source-ghc-pragmas 'ac-source-ghc-langexts))

  ;;; (Bindings) ;;;
  (user/bind-key-local :doc :reference 'hoogle))


(defun user/haskell-mode-hook ()
  "Haskell mode hook."
  (user/generic-haskell-mode-hook)
  (if (el-get-package-is-installed 'hi2)
      (turn-on-hi2)
    (turn-on-haskell-indentation))

  (with-feature 'ghc
    (ghc-init))

  (with-feature 'shm
    (structured-haskell-mode t)))


(defun user/inferior-haskell-mode-hook ()
  "Inferior Haskell mode hook."
  (user/generic-haskell-mode-hook)

  (turn-on-ghci-completion))


(defun user/haskell-mode-init ()
  "Initialize Haskell mode."
  (require-package '(:name haskell-mode))
  (require-package '(:name ghci-completion))
  (require-package '(:name hi2))
  (require-package '(:name flycheck-hdevtools))
  (require-package '(:name structured-haskell-mode))
  (require-package '(:name ghc-mod))
  (require-package '(:name ac-ghc-mod))

  (add-hook 'haskell-mode-hook 'user/haskell-mode-hook)
  (add-hook 'inferior-haskell-mode-hook 'user/inferior-haskell-mode-hook))

(with-executable 'ghc
  (user/haskell-mode-init))


(provide 'modes/haskell)
;;; haskell.el ends here
