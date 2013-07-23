;;; powerline --- powerline mode-line
;;; Commentary:
;;; Code:

(require-package '(:name powerline
			 :type github
			 :pkgname "milkypostman/powerline"
			 :depends (solarized-theme)
			 :after (dholm/powerline-init)
			 :prepare (autoload 'powerline-default-theme "powerline" nil t)))


(defun dholm/powerline-init ()
  (require 'powerline)
  (setq powerline-arrow-shape 'slant)
  (cl-flet ((find-color (name)
			(let* ((index (if window-system
					  (if solarized-degrade 3
					    (if solarized-broken-srgb 2 1))
					(case (display-color-cells)
					  (16 4)
					  (8  5)
					  (otherwise 3)))))
			  (nth index (assoc name solarized-colors)))))
    (set-face-attribute 'mode-line nil
                        :foreground (find-color 'base0) :background (find-color 'base01)
                        :inverse-video nil)
    (set-face-attribute 'mode-line-buffer-id nil
                        :foreground (find-color 'base2) :background (find-color 'blue)
                        :inverse-video nil)
    (set-face-attribute 'mode-line-inactive nil
                        :foreground (find-color 'base0) :background (find-color 'base01)
                        :inverse-video nil)
    (set-face-attribute 'powerline-active1 nil
                        :foreground (find-color 'base2) :background (find-color 'base02)
                        :inverse-video nil)
    (set-face-attribute 'powerline-active2 nil
                        :foreground (find-color 'orange) :background (find-color 'base02)
                        :inverse-video nil)
    (set-face-attribute 'powerline-inactive1 nil
                        :foreground (find-color 'base0) :background (find-color 'base02)
                        :inverse-video nil)
    (set-face-attribute 'powerline-inactive2 nil
                        :foreground (find-color 'base0) :background (find-color 'base01)
                        :inverse-video nil))
  (powerline-default-theme))


(provide 'ux/powerline)
;;; powerline.el ends here
