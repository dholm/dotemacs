;;; deft.el --- sets up Deft
;;; Commentary:
;;; Code:

(defun user/deft-init ()
  "Initialize deft."
  ;;; (Bindings) ;;;
  (user/bind-key-global :apps :notes 'deft))

(require-package '(:name deft :after (user/deft-init)))


(provide 'apps/deft)
;;; deft.el ends here
