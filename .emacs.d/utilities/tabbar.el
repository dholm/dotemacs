;; Enable TabBar minor mode

(push (path-join user-emacs-directory "utilities" "tabbar") load-path)
(require 'tabbar)

(tabbar-mode)
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "M-k") 'tabbar-forward)


(provide 'utilities/tabbar)
