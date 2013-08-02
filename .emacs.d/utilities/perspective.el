;;; perspective --- perspectives in Emacs
;;; Commentary:
;;; Code:

(defun user/perspective-init ()
  "Initialize perspective."
  (define-key user/utilities-map (kbd "p") 'persp-mode))

(require-package '(:name perspective :after (user/perspective-init)))


(provide 'utilities/perspective)
;;; perspective.el ends here
