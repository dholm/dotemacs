;;; recentf --- recent file setup
;;; Commentary:
;;; Code:

(recentf-mode t)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))


(provide 'ux/recentf)
;;; recentf.el ends here
