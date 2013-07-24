;;; solarized --- solarized color theme
;;; Commentary:
;;; Code:

(require-package '(:name solarized-theme
			 :type github
			 :pkgname "awmckinley/solarized-theme"
			 :prepare (add-to-list 'custom-theme-load-path default-directory)
			 :after (dholm/solarized-init)))

(defun dholm/solarized-init ()
  (load-theme 'solarized t))


(provide 'ux/solarized)
;;; solarized.el ends here
