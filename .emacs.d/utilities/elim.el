;;; (Initialization) ;;;
(require-package '(:name elim))


(setq lui-max-buffer-size 30000
      lui-flyspell-p t
      lui-flyspell-alist '(("." "american"))
      elim-directory (path-join *user-cache-directory* "elim"))


(provide 'utilities/elim)
