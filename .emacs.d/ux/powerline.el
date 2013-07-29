;;; powerline --- powerline mode-line
;;; Commentary:
;;; Code:

(defun dholm/powerline-init ()
  "Initialize powerline."
  (require 'powerline)

  ;;; (Faces) ;;;
  (after-load 'solarized-theme
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
         '(powerline-inactive2 ((t (:foreground ,base0 :background ,base01))))))))

  ;;; (Theme) ;;;
  (setq-default powerline-arrow-shape 'slant)
  (powerline-default-theme))

(require-package '(:name powerline
			 :type github
			 :pkgname "milkypostman/powerline"
			 :after (dholm/powerline-init)
			 :prepare (autoload 'powerline-default-theme "powerline" nil t)))


(provide 'ux/powerline)
;;; powerline.el ends here
