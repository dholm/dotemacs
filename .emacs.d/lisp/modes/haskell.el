;;; haskell.el --- initializes Haskell modes
;;; Commentary:
;;; Code:

(defun user--generic-haskell-mode-hook ()
  "Generic Haskell mode hook."
  (turn-on-haskell-doc-mode)

  ;; Enable editing of camel case
  (subword-mode t)

  (user/smartparens-enable)

  (cond
   ((user/auto-complete-p)
    (with-feature 'ac-ghc-mod
      (add-ac-sources 'ac-source-ghc-module 'ac-source-ghc-symbol
                      'ac-source-ghc-pragmas 'ac-source-ghc-langexts)))
   ((user/company-mode-p)
    (with-feature 'company-ghc
      (add-company-sources 'company-ghc))))

  ;;; (Bindings) ;;;
  (user/bind-key-local :doc :reference 'hoogle))


(defun user--haskell-mode-hook ()
  "Haskell mode hook."
  (user--generic-haskell-mode-hook)
  (if (feature-p 'hi2)
      (turn-on-hi2)
    (turn-on-haskell-indentation))

  (with-feature 'ghc
    (ghc-init))

  (with-feature 'shm
    (structured-haskell-mode t)))


(defun user--inferior-haskell-mode-hook ()
  "Inferior Haskell mode hook."
  (user--generic-haskell-mode-hook)

  (turn-on-ghci-completion))


(defun user--haskell-mode-config ()
  "Initialize Haskell mode."
  (after-load 'smartparens
    (defun user/haskell-after-symbol-p (_id action _context)
      (when (eq action 'insert)
        (save-excursion
          (backward-char 1)
          (looking-back "\\sw\\|\\s_\\|\\s'"))))

    (sp-with-modes '(haskell-mode)
      (sp-local-pair "'" nil :unless '(user/haskell-after-symbol-p))
      (sp-local-pair "\\(" nil :actions nil)))

  ;;; (Packages) ;;;
  (use-package haskell-mode
    :ensure t)
  (use-package ghci-completion
    :ensure t)
  (use-package hi2
    :ensure t)
  (use-package flycheck-hdevtools
    :ensure t)
  (use-package flycheck-haskell
    :ensure t)
  (require-package '(:name structured-haskell-mode))
  (require-package '(:name ghc-mod))
  (require-package '(:name ac-ghc-mod))
  (use-package company-ghc
    :ensure t)

  (add-hook 'haskell-mode-hook 'user--haskell-mode-hook)
  (add-hook 'inferior-haskell-mode-hook 'user--inferior-haskell-mode-hook))

(with-executable 'ghc
  (user--haskell-mode-config))


(provide 'modes/haskell)
;;; haskell.el ends here
