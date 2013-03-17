;; Sunrise Commander
(push "~/.emacs.d/utilities/sunrise-commander" load-path)
(require 'sunrise-commander)
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
