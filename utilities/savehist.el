;; Save/restore the history of various Emacs minibuffers

(require 'savehist)
(when (featurep 'savehist)
  (setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (setq savehist-file "~/.emacs.cache/savehist")
  (savehist-mode t))
