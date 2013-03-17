;; Make buffer names unique

(require 'uniquify)
(when (featurep 'uniquify)
  (setq
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))
