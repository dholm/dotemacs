;;; elfeed.el --- Emacs web feed reader.
;;; Commentary:
;;; Code:

(defun user--elfeed-config ()
  "Initialize elfeed."
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :feed-reader 'elfeed))

(use-package elfeed
  :defer t
  :config (user--elfeed-config))


(provide 'apps/elfeed)
;;; elfeed.el ends here
