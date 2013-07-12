;;; (Initialization) ;;;
(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
(setq savehist-file (path-join *user-cache-directory* "savehist"))
(savehist-mode t)


(provide 'ux/savehist)
