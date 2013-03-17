;; SmartTab intelligent tab completion control

(push "~/.emacs.d/utilities/smart-tab" load-path)
(require 'smart-tab)
(when (featurep 'smart-tab)
  (global-smart-tab-mode))
