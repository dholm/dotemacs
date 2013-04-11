(require 'lusty-explorer)
(when (featurep 'lusty-explorer)
  (global-set-key "\C-x\C-f" 'lusty-file-explorer)
  (global-set-key "\C-xb" 'lusty-buffer-explorer))
