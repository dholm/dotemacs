;;; perspective --- perspectives in Emacs
;;; Commentary:
;;; Code:

(defun dholm/perspective-init ()
  "Initialize perspective."
  (define-key dholm/utilities-map (kbd "p") 'persp-mode))

(require-package '(:name perspective :after (dholm/perspective-init)))


(provide 'utilities/perspective)
;;; perspective.el ends here
