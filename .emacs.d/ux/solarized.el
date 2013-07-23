;;; solarized --- solarized color theme
;;; Commentary:
;;; Code:

(require-package '(:name solarized-theme
			 :after (dholm/solarized-init)))

(defun dholm/solarized-init ()
  (load-theme 'solarized-dark t))


(provide 'ux/solarized)
;;; solarized.el ends here
