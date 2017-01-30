;;; sauron.el --- Emacs event tracker
;;; Commentary:
;;; Code:

(defun user--sauron-config ()
  "Initialize Sauron event tracker."
  (setq-default
   ;; Display sauron in current frame.
   sauron-separate-frame nil)

  ;;; (Bindings) ;;;
  (user/bind-key-global :util :notifications 'sauron-toggle-hide-show))

(use-package sauron
  :ensure t
  :config (user--sauron-config))


(provide 'utilities/sauron)
;;; sauron.el ends here
