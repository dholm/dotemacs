;;; recentf.el --- recent file setup
;;; Commentary:
;;; Code:

(recentf-mode t)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:")
 recentf-save-file (path-join *user-cache-directory* "recentf"))


(provide 'ux/recentf)
;;; recentf.el ends here
