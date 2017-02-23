;;; elfeed.el --- Emacs web feed reader.
;;; Commentary:
;;; Code:

(use-package elfeed
  :commands elfeed
  :init
  (user/bind-key-global :apps :feed-reader 'elfeed))


(provide 'apps/elfeed)
;;; elfeed.el ends here
