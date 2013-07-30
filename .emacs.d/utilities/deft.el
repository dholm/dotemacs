;;; deft --- sets up Deft
;;; Commentary:
;;; Code:

(defun dholm/deft-init ()
  "Initialize deft."
  (define-key dholm/utilities-map (kbd "d") 'deft))

(require-package '(:name deft :after (dholm/deft-init)))


(provide 'utilities/deft)
;;; deft.el ends here
