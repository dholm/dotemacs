;;; smart-forward --- smart forward navigation aid
;;; Commentary:
;;; Code:

(defun dholm/smart-forward-init ()
  (define-key dholm/navigation-map (kbd "s f") 'smart-forward)
  (define-key dholm/navigation-map (kbd "s b") 'smart-backward)
  (define-key dholm/navigation-map (kbd "s p") 'smart-up)
  (define-key dholm/navigation-map (kbd "s n") 'smart-down))

(require-package '(:name smart-forward
			 :after (dholm/smart-forward-init)
			 :type elpa
			 :repo ("melpa" . "http://melpa.milkbox.net/packages/")
			 :depends (expand-region)))


(provide 'ux/smart-forward)
;;; smart-forward.el ends here
