;; Magit advanced Git integration

(push "~/.emacs.d/vcs/magit" load-path)
(require 'magit)
(when (featurep 'magit)
  (global-set-key (kbd "C-c m") 'magit-status))
