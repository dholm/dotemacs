;; Enable TabBar minor mode

(push "~/.emacs.d/utilities/tabbar" load-path)
(require 'tabbar)
(when (featurep 'tabbar-mode)
  (tabbar-mode)
  (global-set-key (kbd "M-j") 'tabbar-backward)
  (global-set-key (kbd "M-k") 'tabbar-forward))
