;;; powerline --- powerline mode-line
;;; Commentary:
;;; Code:

(require 'ux/solarized)

(require-package '(:name powerline
			 :type github
			 :pkgname "milkypostman/powerline"
			 :depends (solarized-theme)
			 :after (dholm/powerline-init)
			 :prepare (autoload 'powerline-default-theme "powerline" nil t)))


(defun dholm/powerline-init ()
  (require 'powerline)

  ;;; (Faces) ;;;
  (solarized-with-values
    (eval
     `(custom-theme-set-faces
       'solarized
       '(mode-line ((t (:foreground ,base0 :background ,base01))))
       '(mode-line-buffer-id ((t (:foreground ,base2 :background ,blue))))
       '(mode-line-inactive ((t (:foreground ,base0 :background ,base01))))
       '(powerline-active1 ((t (:foreground ,base2 :background ,base02))))
       '(powerline-active2 ((t (:foreground ,orange :background ,base02))))
       '(powerline-inactive1 ((t (:foreground ,base0 :background ,base02))))
       '(powerline-inactive2 ((t (:foreground ,base0 :background ,base01)))))))

  ;;; (Theme) ;;;
  (setq powerline-arrow-shape 'slant)
  (powerline-default-theme))


(provide 'ux/powerline)
;;; powerline.el ends here
