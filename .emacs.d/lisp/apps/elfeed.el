;;; elfeed.el --- Emacs web feed reader.
;;; Commentary:
;;; Code:

(defun user/elfeed-init ()
  "Initialize elfeed."
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :feed-reader 'elfeed))

(require-package '(:name elfeed :after (user/elfeed-init)))


(provide 'apps/elfeed)
;;; elfeed.el ends here
