;; Save/restore the position in Emacs buffers between sessions

(require 'saveplace)
(when (featurep 'saveplace)
  (setq save-place-file (path-join *user-cache-directory* "saveplace"))
  (setq-default save-place t))


(provide 'utilities/saveplace)
