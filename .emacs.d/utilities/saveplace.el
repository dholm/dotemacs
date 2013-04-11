;; Save/restore the position in Emacs buffers between sessions

(require 'saveplace)
(when (featurep 'saveplace)
  (setq save-place-file "~/.emacs.cache/saveplace")
  (setq-default save-place t))
