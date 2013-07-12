;;; (Initialization) ;;;
(require 'saveplace)
(setq save-place-file (path-join *user-cache-directory* "saveplace"))
(setq-default save-place t)


(provide 'ux/saveplace)
