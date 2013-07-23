;;; smart-forward --- smart forward navigation aid
;;; Commentary:
;;; Code:

(defun dholm/smart-forward-init ()
  (global-set-key (kbd "C-c n s f") 'smart-forward)
  (global-set-key (kbd "C-c n s b") 'smart-backward)
  (global-set-key (kbd "C-c n s p") 'smart-up)
  (global-set-key (kbd "C-c n s n") 'smart-down))

(require-package '(:name smart-forward
			 :after (dholm/smart-forward-init)
			 :type elpa
			 :repo ("melpa" . "http://melpa.milkbox.net/packages/")
			 :depends (expand-region)))


(provide 'ux/smart-forward)
;;; smart-forward.el ends here
