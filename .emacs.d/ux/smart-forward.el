;;; smart-forward --- smart forward navigation aid
;;; Commentary:
;;; Code:

(defun user/smart-forward-init ()
  "Initialize smart-forward."
  (define-key user/navigation-map (kbd "s f") 'smart-forward)
  (define-key user/navigation-map (kbd "s b") 'smart-backward)
  (define-key user/navigation-map (kbd "s p") 'smart-up)
  (define-key user/navigation-map (kbd "s n") 'smart-down))

(require-package '(:name smart-forward :after (user/smart-forward-init)))


(provide 'ux/smart-forward)
;;; smart-forward.el ends here
