;;; smart-forward --- smart forward navigation aid
;;; Commentary:
;;; Code:

(defun dholm/smart-forward-init ()
  "Initialize smart-forward."
  (define-key dholm/navigation-map (kbd "s f") 'smart-forward)
  (define-key dholm/navigation-map (kbd "s b") 'smart-backward)
  (define-key dholm/navigation-map (kbd "s p") 'smart-up)
  (define-key dholm/navigation-map (kbd "s n") 'smart-down))

(require-package '(:name smart-forward
                         :type github
                         :pkgname "magnars/smart-forward.el"
                         :depends (expand-region)
                         :after (dholm/smart-forward-init)))


(provide 'ux/smart-forward)
;;; smart-forward.el ends here
