;;; mingus.el --- MPD frontend for Emacs
;;; Commentary:
;;; Code:

(defun user/mingus-init ()
  "Initialize Mingus."
  (define-key user/utilities-map (kbd "M") 'mingus))

(require-package '(:name mingus :after (user/mingus-init)))


(provide 'apps/mingus)
;;; mingus.el ends here
