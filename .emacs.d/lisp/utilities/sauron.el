;;; sauron.el --- Emacs event tracker
;;; Commentary:
;;; Code:

(defun user/sauron-init ()
  "Initialize Sauron event tracker."
  (setq-default
   ;; Display sauron in current frame.
   sauron-separate-frame nil)

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :notifications 'sauron-toggle-hide-show))

(require-package '(:name sauron :after (user/sauron-init)))


(provide 'utilities/sauron)
;;; sauron.el ends here