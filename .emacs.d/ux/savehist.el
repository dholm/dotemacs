;;; savehist --- Emacs history saving
;;; Commentary:
;;; Code:

(require 'savehist)
(setq savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
(setq savehist-file (path-join *user-cache-directory* "savehist"))
(savehist-mode t)


(provide 'ux/savehist)
;;; savehist.el ends here
