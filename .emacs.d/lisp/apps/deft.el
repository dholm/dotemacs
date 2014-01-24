;;; deft.el --- sets up Deft
;;; Commentary:
;;; Code:

(defun user/deft-init ()
  "Initialize deft."
  (define-key user/utilities-map (kbd "d") 'deft))

(require-package '(:name deft :after (user/deft-init)))


(provide 'apps/deft)
;;; deft.el ends here
