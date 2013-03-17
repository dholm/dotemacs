;; Emacs Code Browser
(require 'ecb)
(when (featurep 'ecb)
  (setq stack-trace-on-error nil)
  (custom-set-variables
   '(ecb-options-version "2.40")))
