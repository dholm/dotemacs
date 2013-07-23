;;; saveplace --- save the last location in open buffers
;;; Commentary:
;;; Code:

(require 'saveplace)
(setq save-place-file (path-join *user-cache-directory* "saveplace"))
(setq-default save-place t)


(provide 'ux/saveplace)
;;; saveplace.el ends here
