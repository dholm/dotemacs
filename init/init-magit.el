;; Magit advanced Git integration

(require 'magit)
(when (featurep 'magit)
  (global-set-key (kbd "C-c m") 'magit-status))
