;;; uniquify --- create unique names for buffers
;;; Commentary:
;;; Code:

(require 'uniquify)
(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator " • "
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")


(provide 'ux/uniquify)
;;; uniquify.el ends here
