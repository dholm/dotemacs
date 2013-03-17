;; ELIM instant messenger

(push "~/.emacs.d/utilities/elim/elisp" load-path)
(autoload 'garak "garak" nil t)

(setq lui-max-buffer-size 30000
      lui-flyspell-p t
      lui-flyspell-alist '(("." "american"))
      elim-directory "~/.emacs.cache/elim"
      elim-executable "~/.emacs.d/utilities/elim/elim-client")
