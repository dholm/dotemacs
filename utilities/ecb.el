;; Emacs Code Browser
(push "~/.emacs.d/utilities/ecb" load-path)
(require 'ecb)
(when (featurep 'ecb)
  (setq stack-trace-on-error nil)
  (custom-set-variables
   '(ecb-options-version "2.40")))
